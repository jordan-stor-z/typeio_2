# pr-screenshots

Screenshots referenced from pull request descriptions, kept on an orphan
branch so the binaries never enter `main`'s history.

This branch is **not** part of the project. It has no code, shares no
history with `main`, and is never merged. It exists because GitHub
renders images in a PR body only from a URL, and a public repository's
own `raw.githubusercontent.com` is the least surprising place to put
them — no third-party host, no expiring link, and they stay next to the
work they document.

One directory per issue. Files are added, never rewritten, so a link in
an old PR keeps resolving to the image that PR was reviewed against.

| Directory | Shows |
|---|---|
| `issue-238/` | The orbital visualization rendering for the first time — before and after a ring-spacing fix that had made every link zero-length |
| `issue-239/` | Per-node colour, and one hover highlighting every replica of a node |
| `issue-244/` | One title edit updating both replicas of a node, wrapped to the circle's width |
