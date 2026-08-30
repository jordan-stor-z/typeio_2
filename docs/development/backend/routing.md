# Routing

There is no routing library here — `Platform.Web.Router` is a
purpose-built router on top of a small trie data structure
(`Data.HashTree`). This doc explains both, since neither is discoverable
by name the way a library would be.

## `Data.HashTree`: the data structure

```haskell
data HashTree k a = Branch (H.HashMap k (HashTree k a)) | Node a
```

A trie keyed by anything `Hashable` — here, `Text` path segments at the
top level, and `Method` (HTTP method) one level below each resolved path.
Three combinators build one:

```haskell
(<+>) :: Hashable k => HashTree k a -> k -> HashTree k a -> HashTree k a
(-|)  :: (HashTree k a -> HashTree k a) -> a -> HashTree k a
(-<)  :: (HashTree k a -> HashTree k a) -> HashTree k a -> HashTree k a
```

`t <+> key` is a partially-applied insert: it's a function still waiting
for what to put at `key`. `-|` finishes it with a **leaf value** (wraps it
in `Node`); `-<` finishes it with an **already-built subtree** (a
`Branch`, i.e. more routes nested underneath). That's the whole
vocabulary — read `<+> "foo" -| x` as "leaf `x` at `foo`" and `<+> "foo"
-< t` as "the whole subtree `t` nested at `foo`":

```haskell
apiTree :: RootContainer -> Request -> RouteTree
apiTree ctn req = emptyT
  <+> "central" -< centralApiTree ctrCtn      -- nested subtree
  <+> "project" -< projectApiTree prjCtn req
  <+> "system"  -< systemApiTree  sysCtn req

centralApiTree :: CentralApiContainer -> RouteTree
centralApiTree ctn = emptyT
  <+> "seed-database" -| only "POST" (apiSeedDatabase ctn)  -- leaf
```

Lookup (`findPath`) walks the path segments one at a time, following
branches, and **stops the instant it reaches a `Node`** — regardless of
how many path segments are left unconsumed:

```haskell
findPath :: Hashable k => [k] -> HashTree k a -> Maybe a
findPath _        (Node x)     = Just x
findPath []       _            = Nothing
findPath (p : ps) (Branch h)   = H.lookup p h >>= findPath ps
```

**This means routes match on a path *prefix*, not the full path.** A
request to `/api/central/seed-database/anything/else` resolves exactly
the same as `/api/central/seed-database` — the extra segments are simply
never looked at. There's no trailing-slash or exact-match validation
happening anywhere in this router. If a handler ever needs to reject
extra path segments, it has to do that itself; the router won't.

## `Platform.Web.Router`: the route tree

```haskell
type RouteTree  = HashTree Text MethodTree
type MethodTree = HashTree Method ((Response -> IO ResponseReceived) -> IO ResponseReceived)
```

Two levels: path segments resolve to a `MethodTree`, which then resolves
by HTTP method to the actual handler action. `routeRequest` does both
lookups in sequence and falls back to a generic 404 if either misses:

```haskell
routeRequest ctn req = fromMaybe (notFound req) $
  findPath pth (rootTree ctn req) >>= findPath [mth]
  where
    pth = pathInfo req <|> [""]
    mth = requestMethod req
```

`pathInfo req <|> [""]` is there for exactly one case: WAI's `pathInfo`
for `GET /` is `[]`, and the root route is registered at key `""`
(`rootTree`'s `<+> "" -| only "GET" (index ...)`). `[] <|> [""]` (list
`Alternative` is concatenation) gives `[""]`, which matches. For any
non-root path this appends a harmless trailing `""` that's never reached,
because `findPath` already stopped at the `Node` — see the prefix-match
note above.

The whole tree (`rootTree`, `apiTree`, `uiTree`, ...) is **rebuilt on
every incoming request** — it's not a static table built once at startup.
That's why several of the builder functions take `Request` as a
parameter: routes whose handler needs to read the request directly (e.g.
`ProjectApi.postNode ctn req`, which is a `Container` field typed
`Application` — `Request -> (Response -> IO ResponseReceived) -> IO
ResponseReceived` — partially applied to `req` right there in the tree)
close over it at tree-construction time. This is cheap: the tree itself
is a handful of hash-map inserts over small, fixed sets of string/method
keys, not something that needs caching.

## Adding a route

1. Add the handler to the relevant domain's `Container` (see
   [containers.md](containers.md)) if it isn't there already.
2. Add a `<+> "segment" -| only "METHOD" (yourHandler ctn)` (or `-<` a
   nested subtree, for a path with more segments underneath) at the
   right point in `Platform.Web.Router`'s tree-building functions
   (`rootTree`, `apiTree`, `uiTree`, or one of their sub-trees).
3. There's no separate route registry to update — the tree in
   `Platform.Web.Router` *is* the registry.
