include "root" {
  path = find_in_parent_folders()
}

terraform {
  source = "../../../modules/github-repo"
}

inputs = {
  repository = "typeio_2"
  branch     = "main"

  required_status_check_contexts  = ["test"]
  required_status_check_strict    = true
  required_approving_review_count = 0
  dismiss_stale_reviews           = false
  require_code_owner_reviews      = false
  enforce_admins                  = true
  allow_force_pushes              = false
  allow_deletions                 = false
}
