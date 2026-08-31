# Infrastructure

Repo-level configuration that used to be set up by hand via the GitHub
API/UI (branch protection on `main`, currently) is managed as code under
`infrastructure/`, using [OpenTofu](https://opentofu.org/) + Terragrunt.
This is the first piece of what may grow into environment-based cloud
infrastructure later — no cloud provider is chosen yet, so today this
covers GitHub repo config only.

**OpenTofu, not Terraform.** Same HCL, same workflow, same
`integrations/github` provider (it resolves the same on OpenTofu's
registry) — but the CLI binary is `tofu`, not `terraform`, and
`infrastructure/live/github/typeio_2/terragrunt.hcl` sets
`terraform_binary = "tofu"` so Terragrunt invokes it. Install it with
`brew install opentofu`. **Terragrunt must be `>= 1.1`** to work with
OpenTofu at all — `0.43.2` fails `terragrunt init` outright with
`Unable to parse Terraform version output: OpenTofu v1.12.6`, because it
only knows how to parse `terraform version`'s output format. `brew
upgrade terragrunt` if you hit that.

## Layout

- `infrastructure/modules/` — reusable modules. Nothing here is
  hardcoded to one repo/environment; modules take everything
  repo/environment-specific as an input variable.
  - `github-repo/` — wraps `github_branch_protection`, parameterized by
    repository, branch pattern, required status checks, required
    approving review count, `enforce_admins`, and force-push/deletion
    policy.
- `infrastructure/live/` — Terragrunt configs: the actual
  environment/target-specific values plugged into a module. Grouped by
  concern (`live/github/...`) rather than a flat list, so a future
  target (e.g. a cloud provider's environments) can be added alongside
  without reshuffling what's already here.
  - `live/github/typeio_2/terragrunt.hcl` — instantiates
    `modules/github-repo` with this repo's actual live values, and owns
    its own provider/backend config directly (via `generate` blocks).
    There's deliberately no shared root `terragrunt.hcl` yet — with only
    one live GitHub config, an `include`-based parent config that exists
    purely to be shared has nothing to share. Reintroduce one (root
    `live/github/terragrunt.hcl` + `include "root"` in each child) once
    a second config under `live/github/` actually needs the same
    provider/backend wiring.

## State backend: HCP Terraform (Terraform Cloud), free tier

No cloud provider has been chosen yet, so there's no natural home for a
remote backend (S3, GCS, etc.), and a local state file checked into git
is not an option — it'd have to hold live GitHub resource IDs, could
diverge from reality with no locking, and there is no CI runner set up
yet to be the sole `apply`r of a local file safely.
[HCP Terraform](https://app.terraform.io)'s free tier was chosen because
it's provider-agnostic (doesn't force a cloud choice just to get remote
state) and gives state locking + history for free without standing up
any infrastructure of its own to hold infrastructure state. As of 2026
the free tier is usage-based (up to 500 managed resources, unlimited
users, one concurrent run) rather than the older 5-user cap — comfortably
enough for this project's single `github_branch_protection` resource.

OpenTofu's `cloud` block genuinely talks to HCP Terraform (confirmed by
running `tofu init` against `hostname = "app.terraform.io"` and getting
a real `tofu login app.terraform.io` prompt, not a rejection) — but
unlike Terraform, it does **not** default `hostname` to
`app.terraform.io` when omitted, so it's set explicitly in the `cloud`
block below.

One-time setup (not automated — this is an account/workspace the
project's HCP Terraform organization owns, not something `tofu apply`
creates):

1. Create an HCP Terraform organization (or reuse an existing one) and
   update `organization` in
   `infrastructure/live/github/typeio_2/terragrunt.hcl` if it differs
   from `typeio-2`.
2. Create a workspace named `typeio_2` with **CLI-driven** workflow (not
   VCS-driven) — Terragrunt runs `tofu` locally/in CI and just uses HCP
   Terraform for state.
3. `tofu login app.terraform.io` (or set `TF_TOKEN_app_terraform_io` in
   CI) so the local/CI `tofu`/`terragrunt` can authenticate to the
   workspace.

## GitHub provider authentication

The `integrations/github` provider reads `GITHUB_TOKEN` from the
environment natively — it is never set in any `.tf`/`.hcl` file. Export
it (a PAT or GitHub App token with admin rights on the target repo)
before running any `terragrunt` command:

```sh
export GITHUB_TOKEN=<token with repo admin scope>
```

## Importing the existing branch protection

`infrastructure/live/github/typeio_2` was written to match what's
actually live on `main` today (required PR, 0 required approvals,
required status check `test`, strict, `enforce_admins`, no force
pushes, no deletions) — applying without importing first would try to
create a branch protection rule that already exists. Import it into the
HCP Terraform-backed state once, after the workspace setup above:

```sh
cd infrastructure/live/github/typeio_2
terragrunt import github_branch_protection.this typeio_2:main
terragrunt plan   # must show "No changes."
```

`repository_id` in the module is resolved via a `github_repository`
data source lookup (not passed as the repo name directly) — the
provider normalizes it to the repo's GraphQL node ID on read, so
passing the name would show a spurious destroy/recreate on every plan
after import.

This exact sequence (import + zero-diff plan, plus the `repository_id`
fix above) was verified against the real, live `main` branch protection
during development, using a temporary local-backend override that was
never committed, before the HCP Terraform workspace existed — the steps
above are what remains to run once it does.

## Day to day

```sh
cd infrastructure/live/github/typeio_2
terragrunt plan
terragrunt apply
```
