output "branch_protection_id" {
  description = "ID of the created github_branch_protection resource."
  value       = github_branch_protection.this.id
}
