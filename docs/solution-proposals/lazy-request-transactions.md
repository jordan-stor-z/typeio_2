# Solution Proposal: Lazy, Request-Scoped Transactions Shared Across Domains

- **Status: Decided against.** See §7 — kept here as a record of the
  analysis and why it was rejected, not as guidance to build any of this.
- **Date:** 2026-08-30
- **Related:** [#50](https://github.com/v12-Industry/typeio_2/issues/50)
  (this spike),
  [#42](https://github.com/v12-Industry/typeio_2/issues/42) /
  `integration-testing.md` (the testing limitation that prompted this),
  `docs/development/backend/{routing,containers,environment}.md`

## 1. Problem statement

Domains in this app are schemas within one Postgres database, not
separate datastores — the team wants to keep leveraging one real
Postgres for cross-domain consistency rather than modeling domain
boundaries as if they might become separate services later. That means
a single request that touches more than one domain should be able to
commit or roll back as one atomic unit.

Today it can't. Every responder calls `runSqlPool` directly (e.g.
`handlePostNode`'s `flip runSqlPool pl . runEitherT $ ...`), and
`runSqlPool` opens *and commits* its own transaction around whatever
it's given — so the transaction boundary is, incidentally, always
exactly one responder. This is what
[#42](https://github.com/v12-Industry/typeio_2/issues/42)'s
integration-testing spike ran into ("wrap a test in a transaction and
roll it back" doesn't work, because there's no outer transaction to roll
back). That's a symptom; this document is about the actual cause — see
§7 for why the conclusion below turned out not to require fixing that
cause after all.

Two requirements, both need to hold:

1. A transaction should be able to span domains within one request.
2. Opening a transaction must not be an eager per-request cost — most
   responders don't touch the database at all and shouldn't pay for a
   connection checkout + `BEGIN` they never use.

## 2. Why not the obvious places first

**Not the existing `Container`s.** `RootContainer` and every domain
container are built exactly once, at process startup
(`Container.Build.withRootContainer`), and reused for every request for
the life of the process — see
[`containers.md`](../development/backend/containers.md). A per-request
value (even a mutable one) doesn't have anywhere to live in something
built once and shared across all concurrent requests; two requests would
share the same cell and race on the same connection. This isn't a style
preference to work around — a per-request value structurally cannot live
in a per-process container.

**Not WAI's `vault`, for now.** `vault` is WAI's mechanism for a
middleware to make a per-request value visible to code deeper in the
chain without changing everyone's type signature — genuinely the right
tool when a `Middleware` (fixed as `Application -> Application`, unable
to carry an extra explicit parameter) needs to share state with the
router/handler. But no database-touching middleware exists yet, and
`vault` doesn't actually save the signature churn it usually promises in
*this* codebase specifically: several handlers today don't take
`Request` at all (`ProjectApi.getNodes :: (Response -> IO
ResponseReceived) -> IO ResponseReceived`, no `Request` parameter,
because a plain `GET` has nothing to parse) — reading from `vault`
requires a `Request`, so those handlers would need a signature change to
reach it anyway, the same cost as just adding an explicit parameter.
`vault`'s only genuine advantage here is bridging *middleware* to
handlers specifically, and that need doesn't exist yet. Defer it until
it does.

## 3. Lazy-open mechanism

Haskell offers two ways to defer opening a connection until first use.
Both were considered.

**Not `unsafeInterleaveIO`.** `System.IO.Unsafe.unsafeInterleaveIO ::
IO a -> IO a` defers running an action until its result is demanded, and
memoizes it after that — a real technique (it's how lazy `getContents`
works), and it would technically work here. Rejected because "unsafe"
isn't decorative: *when* the action runs becomes tied to GHC's
evaluation/forcing order rather than program order, and that's a real
risk for something as consequential as opening a transaction. Prefer
determinism for side effects with this much weight.

**Recommended: an explicit, memoized `IORef`.** Same practical effect —
opens once, on first use, reused after that — with a plain, inspectable
branch instead of implicit thunk-forcing semantics:

```haskell
-- Environment.Transaction

newtype LazyConn = LazyConn (IORef (Maybe SqlBackend))

newLazyConn :: IO LazyConn
newLazyConn = LazyConn <$> newIORef Nothing

-- | Get this request's SqlBackend, checking out a connection and
-- starting a transaction on first use. Subsequent calls within the same
-- request reuse it -- no new connection, no new BEGIN.
getConn :: ConnectionPool -> LazyConn -> IO SqlBackend
getConn pool (LazyConn ref) = readIORef ref >>= \case
  Just conn -> pure conn
  Nothing   -> do
    conn <- checkOutAndBegin pool
    writeIORef ref (Just conn)
    pure conn
```

`persistent` doesn't force going through `runSqlPool`'s convenience
wrapper to do this — `SqlBackend` exposes `connBegin`/`connCommit`/
`connRollback` directly (`Database.Persist.SqlBackend.Internal`), and
`resource-pool` exposes manual checkout/return
(`Data.Pool.takeResource`/`putResource`/`destroyResource`) alongside the
bracketed `withResource` the codebase already uses elsewhere. Verified
against the actual installed versions (`persistent-2.18.1.0`,
`resource-pool-0.5.1.0`):

```haskell
checkOutAndBegin :: ConnectionPool -> IO SqlBackend
checkOutAndBegin pool = do
  (conn, _localPool) <- takeResource pool
  connBegin conn (connPrepare conn) Nothing  -- Nothing = default isolation level
  pure conn
```

(The `LocalPool` from `takeResource` needs to be threaded through to
whatever eventually calls `putResource`/`destroyResource` — see the
commit/rollback wrapper in §5, which is the natural place to return the
connection to the pool once the request is done with it.)

## 4. Threading it through: bigger than it first looked

The plan discussed was "thread it like `req` already flows." Looking at
`Platform.Web.Router` closely, that undersells the actual size of this
change, and it's worth being honest about that here rather than
discovering it mid-implementation.

**What's true today:** route-tree functions (`rootTree`, `apiTree`,
`projectApiTree`, ...) already take `Request` and are rebuilt fresh per
request (see [`routing.md`](../development/backend/routing.md)) — but
the *handlers* they call are a mix. Some are already-baked closures
taking no extra arguments at all:

```haskell
-- Domain.Project.Responder.Api.Container
getNodes :: (Response -> IO ResponseReceived) -> IO ResponseReceived
```

Others are full `Application`, explicitly applied to `req` right there
in the router:

```haskell
<+> "nodes" -|
  ( methods
    <+> "GET"  -| ProjectApi.getNodes ctn        -- no req needed
    <+> "POST" -| ProjectApi.postNode ctn req    -- req applied here
  )
```

So `req` isn't uniformly threaded to every handler today — only the ones
that need it. Adding `LazyConn` to every DB-touching handler means each
of those container fields grows a parameter regardless of whether it
currently takes `Request`, and every intermediate tree-building function
(`apiTree`, `projectApiTree`, `uiTree`, `manageProjectUiTree`, ...) needs
to accept and pass through a `LazyConn`, purely to hand it to the leaves
— the tree structure itself doesn't care about it.

**Sketch of the shape** (not a full transformation of every handler —
enough to show the pattern):

```haskell
-- Platform.Web.hs
app :: RootContainer -> ConnectionPool -> Application
app ctn pool req respond = do
  lc <- newLazyConn
  runRequestTransaction pool lc $
    routeRequest ctn lc req respond

-- Platform.Web.Router.hs
routeRequest :: RootContainer -> LazyConn -> Request
  -> ((Response -> IO ResponseReceived) -> IO ResponseReceived)
routeRequest ctn lc req = fromMaybe (notFound req) $
  findPath pth (rootTree ctn lc req) >>= findPath [mth]
  where pth = pathInfo req <|> [""]; mth = requestMethod req

apiTree :: RootContainer -> LazyConn -> Request -> RouteTree
apiTree ctn lc req = emptyT
  <+> "project" -< projectApiTree prjCtn lc req
  -- ...

projectApiTree :: ProjectApi.Container -> LazyConn -> Request -> RouteTree
projectApiTree ctn lc req = emptyT
  <+> "nodes" -|
    ( methods
      <+> "GET" -| ProjectApi.getNodes ctn lc   -- now takes LazyConn
    )
  -- ...

-- Domain.Project.Responder.Api.Node.Get
handleGetNodes :: ConnectionPool -> LazyConn
  -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGetNodes pool lc respond = do
  conn <- getConn pool lc          -- opens on first real use
  ns <- encode . map toSchema <$> runReaderT query conn
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
```

**Sizing this honestly**: propagating `LazyConn` to *every* DB-touching
handler and every intermediate tree function across all of `Platform.Web.Router`
and every domain's `Container` is a real, repo-wide mechanical change —
comparable in shape (though not in intent) to the directory-rename fix
from [#41](https://github.com/v12-Industry/typeio_2/issues/41), just
across Haskell function signatures instead of file paths. This spike is
not proposing to do that everywhere in one pass; a follow-up
implementation ticket would need to scope it (likely: the shared
transaction plumbing first, then migrate handlers domain-by-domain, the
same narrow-first pattern used for
[#28](https://github.com/v12-Industry/typeio_2/issues/28) →
[#29](https://github.com/v12-Industry/typeio_2/issues/29)–[#34](https://github.com/v12-Industry/typeio_2/issues/34)).

## 5. Commit / rollback policy — refined from the original discussion

The original framing was "responders need an explicit way to signal
rollback, since the current validate-before-write discipline won't hold
once the transaction is shared." That's not quite right, and the more
precise version matters for what actually needs building:

**Single-responder validate-then-write discipline still works unchanged.**
`handlePostNode`'s `EitherT`-based flow — validate, query prerequisites,
*then* insert — doesn't write anything on a failing path today, and
nothing about *who owns the transaction* changes that. Swapping
`runSqlPool pl` for `runReaderT conn` inside one responder changes
nothing about this; a `Left` returned before any `insert` still means
nothing was written, regardless of whether the surrounding transaction
is responder-scoped or request-scoped.

**The actual new risk is cross-domain**: Domain A writes successfully,
then a later step (a different domain, sharing the same request
transaction) fails. Today that's a non-issue because Domain A's write
was already committed in its own separate transaction by the time Domain
B runs at all. Once they share one transaction, an explicit answer is
needed: should Domain A's already-executed write be rolled back too? For
this proposal's stated goal (cross-domain atomicity, "ensures successful
database states") — yes, that's the entire point of sharing a
transaction, so it should.

**Mechanism: an explicit exception, not a generic "commit unless
something threw" policy.**

```haskell
-- Environment.Transaction
newtype TransactionAbort = TransactionAbort Text
  deriving Show
instance Exception TransactionAbort
```

Any code that needs to guarantee "if this fails, roll back the whole
request" — most relevantly, a later domain's failure after an earlier
domain already wrote in the same request — throws `TransactionAbort`
rather than only returning a normal `Left`. The request wrapper commits
on a normal return and rolls back if a `TransactionAbort` (or any other
exception) propagates out:

```haskell
runRequestTransaction :: ConnectionPool -> LazyConn -> IO r -> IO r
runRequestTransaction pool lc@(LazyConn ref) action =
  action `onException` rollbackIfOpened
    <* commitIfOpened
  where
    rollbackIfOpened = readIORef ref >>= mapM_ (\c -> connRollback c (connPrepare c))
    commitIfOpened   = readIORef ref >>= mapM_ (\c -> connCommit  c (connPrepare c))
```

(Illustrative — the real version needs to also return the connection to
the pool via `putResource`/`destroyResource`, and `onException`/`<*`
ordering needs care so a commit is never attempted after a rollback
already ran. Worth writing as a small, separately-tested module rather
than inlining this by hand at each call site.)

## 6. Connection-pool sizing

A shared, request-scoped transaction can hold a connection open across
more of a request's wall-clock time than today's narrow per-responder
`runSqlPool` scope (e.g. if other work happens between two domains'
writes within the same request). This puts more pressure on
`DB_POOL_COUNT` (`Config.Db`) than the current model does. Not something
to solve in this spike — flagging it as a real, measurable thing to
watch once this is implemented and under real load, not a theoretical
concern to dismiss.

## 7. Decision

**Decided against, 2026-08-30**, after further thought past what's
written above — reconsidering what "cross-domain" actually means for
this app rather than the mechanism for achieving it.

The premise of this whole document was that cross-domain atomicity
requires *lifting the transaction boundary out of the responder* —
because a shared transaction would need to span multiple responders, or
middleware, coordinating with each other. But that's not actually how
cross-domain composition happens here, or is expected to happen going
forward. `Domain.Central` already exists specifically as the place where
multiple domains combine to serve a UI view. As the app grows — a `User`
domain, UI modules migrating into `Central` as they currently sit under
`Domain.Project` — the natural shape is that **one responder in
`Central` calls directly into other domains' query/write functions**,
all within its own single call stack. That responder can open its own
`runSqlPool` transaction exactly the way every responder already does
today, and every domain function it calls participates in that same
transaction for free, because it's all one Haskell function running one
`ReaderT SqlBackend IO` action — no architecture change needed at all.

**Only one responder ever handles a request** was the original
assumption behind the current design, and it still holds even once a
single request's work spans multiple domains — it just means that one
responder's code directly composes calls into more than one domain,
rather than the framework needing to coordinate multiple responders or
middleware sharing a lifted-out transaction. `Container`s already fit
this: they can simply expose query/write functions from various domains
for a `Central` responder to call directly, alongside the domain's own
containers doing the same for their own responders. That's a much
smaller, more Locality-of-Behavior-aligned answer than everything in
§§2–6 above — the whole lazy-open/vault/rollback-signaling design was
solving a coordination problem that doesn't actually exist once
cross-domain composition is understood as "one responder, several domain
function calls" rather than "several responders, one shared transaction."

Sections 1–6 are kept as-is above as the record of that analysis and why
it doesn't hold up, not as a design to build from. Nothing in this
document should be implemented.

## 8. Open questions

None — moot, given §7. The naming/isolation-level/test-isolation
questions that would have followed from actually building this no
longer apply. (The note added to
`docs/solution-proposals/integration-testing.md` pointing at this
document as a dependency has been corrected back — its original
"truncate between tests" recommendation stands unmodified.)
