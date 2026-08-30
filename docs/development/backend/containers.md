# Containers

This project's form of dependency injection: instead of a typeclass-based
effects system (`mtl`-style constraints, `ReaderT`-over-`IO`, etc.),
dependencies are plain records of already-applied functions and values,
built once and passed down explicitly. If [Environment](environment.md)
is *what raw resources exist* (config, logger, DB pool), Containers is
*how those resources get shaped into exactly what each handler needs*.

## The hierarchy

```
RootContainer                                (Container.Root)
├── central : CentralContainer                (Domain.Central.Container)
│   ├── centralApiContainer : CentralApiContainer
│   └── centralUiContainer  : Container
├── project : ProjectContainer                (Domain.Project.Container)
│   ├── projectApiContainer' : Container
│   └── projectUiContainer'  : Container
└── system  : SystemContainer                 (Domain.System.Container)
    ├── middleware : Container
    └── responder  : Container
```

Built top-down in `Container.Build.withRootContainer`, from an `Env`
(the `ConnectionPool`, `EntryLog`, and `AppConfig` — see
[environment.md](environment.md)):

```haskell
withRootContainer :: Env -> (RootContainer -> IO a) -> IO a
withRootContainer ev k = k RootContainer
  { appConfig = appConf ev
  , central   = CentralContainer.defaultContainer  pl
  , project   = ProjectContainer.defaultContainer  pl
  , system    = SystemContainer.defaultContainer (appConf ev) lg
  }
  where lg = logger ev
        pl = pool ev
```

Each domain container's own `defaultContainer` does the same thing one
level further down — split into an **API sub-container** and a **UI
sub-container**, each holding just the handler functions that side of
the domain needs, already partially applied with the pool/config they
close over:

```haskell
-- Domain.Project.Responder.Api.Container
data Container = Container
  { getNodes        :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , getNodeStatuses :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , getNodeTypes    :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , getProjects     :: (Response -> IO ResponseReceived) -> IO ResponseReceived
  , postNode        :: Application
  }

defaultContainer :: ConnectionPool -> Container
defaultContainer cpl = Container
  { getNodes        = handleGetNodes cpl
  , getNodeStatuses = handleGetNodeStatuses cpl
  , ...
  }
```

Every field is just a handler function with its dependencies (usually
just the `ConnectionPool`) already baked in via partial application —
there's no runtime resolution, no `ask`, no typeclass dispatch. A
container is a plain value; getting a handler out of it is a record
field access.

`Domain.System.Container` is the odd one out structurally: instead of
API/UI, it splits into `middleware` (`Domain.System.Middleware.Container`
— the request-id/logging middleware functions, see
[logging.md](logging.md)) and `responder` (`Domain.System.Responder.Container`
— the `/api/system/config` handler). Same pattern, different names for
the two halves, because the system domain doesn't have a UI.

## How it reaches a handler

[`Platform.Web.Router`](routing.md) is handed the `RootContainer` and
pulls the specific sub-container/field it needs for each route as it
builds the route tree, e.g.:

```haskell
projectApiTree ctn req = emptyT
  <+> "nodes" -|
    ( methods
      <+> "GET"  -| ProjectApi.getNodes ctn
      <+> "POST" -| ProjectApi.postNode ctn req
    )
  where ctn = ... -- the domain's API container, pulled off RootContainer
```

`ProjectApi.getNodes ctn` is just the record accessor applied to the
container — the handler function itself, ready to run.

## Adding a dependency

1. Add the handler (or whatever the dependency is) as a field on the
   relevant `Container` record (API, UI, or the domain container itself
   if it's a new sub-container).
2. Wire it up in that container's `defaultContainer`, applying whatever
   from `Env`/a parent container it needs.
3. Pull the new field off the container wherever it's needed — most
   commonly from [`Platform.Web.Router`](routing.md) when adding a route,
   but containers can also be passed to other containers (see
   `ProjectContainer.defaultContainer` passing `pl` into both its API and
   UI sub-containers).

There's no DI framework doing wiring for you — every level of this
hierarchy is wired by hand, which is the tradeoff for not having any
typeclass machinery: adding a dependency means touching every container
between where it's produced and where it's consumed, but it's always
possible to read exactly where a handler's dependencies came from by
following plain function calls.
