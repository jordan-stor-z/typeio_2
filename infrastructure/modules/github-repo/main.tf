# The provider normalizes repository_id to the repo's GraphQL node ID on
# read, so passing the repo name directly here would force a
# destroy/recreate on every plan after import. Look it up instead.
data "github_repository" "this" {
  name = var.repository
}

resource "github_branch_protection" "this" {
  repository_id = data.github_repository.this.node_id
  pattern       = var.branch

  enforce_admins      = var.enforce_admins
  allows_deletions    = var.allow_deletions
  allows_force_pushes = var.allow_force_pushes

  required_status_checks {
    strict   = var.required_status_check_strict
    contexts = var.required_status_check_contexts
  }

  required_pull_request_reviews {
    dismiss_stale_reviews           = var.dismiss_stale_reviews
    require_code_owner_reviews      = var.require_code_owner_reviews
    required_approving_review_count = var.required_approving_review_count
  }
}
