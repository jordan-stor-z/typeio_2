locals {
  github_owner = "jordan-stor-z"
}

# GitHub provider authentication is via the GITHUB_TOKEN environment
# variable, which the provider reads natively -- never hardcode it here.
# See docs/development/infrastructure.md.
generate "provider" {
  path      = "provider.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
provider "github" {
  owner = "${local.github_owner}"
}
EOF
}

# Remote state: HCP Terraform (Terraform Cloud) free tier -- see
# docs/development/infrastructure.md for why, and for one-time workspace
# setup.
generate "cloud" {
  path      = "cloud.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
terraform {
  cloud {
    organization = "typeio-2"

    workspaces {
      name = "${replace(path_relative_to_include(), "/", "-")}"
    }
  }
}
EOF
}
