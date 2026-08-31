# OpenTofu, not Terraform -- see docs/development/infrastructure.md.
terraform_binary = "tofu"

terraform {
  source = "../../../modules/github-repo"
}

# GitHub provider authentication is via the GITHUB_TOKEN environment
# variable, which the provider reads natively -- never hardcode it here.
# See docs/development/infrastructure.md.
generate "provider" {
  path      = "provider.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
provider "github" {
  owner = "jordan-stor-z"
}
EOF
}

# Where Terraform/OpenTofu state for this config lives: HCP Terraform
# (Terraform Cloud) free tier, not local state and not checked into git --
# see docs/development/infrastructure.md for why, and for one-time
# workspace setup. Unlike Terraform, OpenTofu doesn't default `hostname`
# to app.terraform.io, so it's set explicitly here.
generate "cloud" {
  path      = "cloud.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
terraform {
  cloud {
    hostname     = "app.terraform.io"
    organization = "typeio-2"

    workspaces {
      name = "typeio_2"
    }
  }
}
EOF
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
