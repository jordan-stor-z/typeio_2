variable "repository" {
  description = "Name of the GitHub repository (not owner/repo, just the repo name) to apply branch protection to."
  type        = string
}

variable "branch" {
  description = "Branch name pattern to protect (e.g. \"main\")."
  type        = string
  default     = "main"
}

variable "required_status_check_contexts" {
  description = "Status check contexts that must pass before merging (e.g. [\"test\"])."
  type        = list(string)
  default     = []
}

variable "required_status_check_strict" {
  description = "Require the branch to be up to date with the base branch before merging."
  type        = bool
  default     = true
}

variable "required_approving_review_count" {
  description = "Number of approving reviews required before merging. 0 means a PR is required but no specific approval count is enforced."
  type        = number
  default     = 1

  validation {
    condition     = var.required_approving_review_count >= 0 && var.required_approving_review_count <= 6
    error_message = "required_approving_review_count must be between 0 and 6."
  }
}

variable "dismiss_stale_reviews" {
  description = "Dismiss approving reviews automatically when a new commit is pushed."
  type        = bool
  default     = false
}

variable "require_code_owner_reviews" {
  description = "Require an approving review from a CODEOWNERS-designated reviewer."
  type        = bool
  default     = false
}

variable "enforce_admins" {
  description = "Enforce all configured restrictions for administrators too."
  type        = bool
  default     = true
}

variable "allow_force_pushes" {
  description = "Permit force pushes to the protected branch."
  type        = bool
  default     = false
}

variable "allow_deletions" {
  description = "Permit the protected branch to be deleted."
  type        = bool
  default     = false
}
