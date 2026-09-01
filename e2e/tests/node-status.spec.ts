import { test, expect } from '@playwright/test';
import { addNode, createProject } from './helpers';

// Third workflow covered by this suite (#96, follow-up to #94/#95):
// change a node's status via the status <select> in the node-detail
// panel, and confirm it both updates the panel's own indicator and
// actually persists (re-fetch the panel and check the new status is
// selected there too, not just that the indicator claimed success).
//
// Same deep-link approach as #95's edit-node.spec.ts for opening the
// panel (?nodeId= in the URL, not clicking the D3 graph) -- see that
// spec's comments for why.
test("changing a node's status updates and persists it", async ({ page, request }) => {
  const project = await createProject(page, 'E2E node-status project');
  const node = await addNode(request, project.id, 'E2E node-status');

  await page.goto(`/ui/project/vw?projectId=${project.id}&nodeId=${node.id}`);
  await page.getByRole('button', { name: 'mode_edit' }).click();

  // A new node is seeded "active" (Api.Node.Post.handlePostNode always
  // queries the "active" NodeStatus). Pick a different one from the
  // same reference data `make seed-db` provides
  // (Domain.Central.Responder.Api.Seed.nodeStatuses).
  //
  // click() before selectOption(), not selectOption() alone: confirmed
  // by direct testing that calling selectOption() on this <select>
  // immediately after it's htmx-swapped into the DOM never fires its
  // hx-trigger="change" PUT -- no request, ever. Explicitly clicking
  // the element first (even though selectOption() doesn't require a
  // prior click to work in general) makes it fire reliably every time;
  // an artificial wait between the two does not fix it on its own, so
  // this isn't a settle-timing issue -- see e2e/README.md's Notes for
  // the general shape of this hazard (freshly htmx-swapped-in elements
  // and Playwright's non-pointer interaction helpers).
  const status = page.locator('select[name="status"]');
  await status.click();
  await status.selectOption('closed');

  // Assert on the settled success icon (Node.Status.templatePostSuccess's
  // literal "done" ligature) in #status-indicator, per the proposal's
  // "never assert mid-swap" convention.
  await expect(page.locator('#status-indicator i.material-icons')).toHaveText('done');

  // Confirms it actually persisted, not just that the indicator claimed
  // success -- via the plain (non-edit) node-detail view's Status text,
  // not by reopening the edit dropdown and checking its selected
  // option. Found a real bug writing this: the edit dropdown
  // (Node.Edit.templateNodeEdit) sets `selected` on the <select>
  // element itself, not on the matching <option> -- not meaningful
  // HTML, so no browser ever shows the real current status there
  // regardless of what's actually in the database (it always shows
  // whichever <option> comes first, "active", since none of them
  // carry `selected`). Confirmed directly against the server's raw
  // HTML response, not a Playwright/browser quirk. Flagged as its own
  // ticket rather than worked around silently -- see the PR
  // description. The plain detail view renders status as text driven
  // directly by the same DB column and isn't affected by that bug, so
  // it's still a faithful persistence check.
  await page.getByRole('button', { name: 'check' }).click();
  const statusRow = page.locator('#node-detail #node-properties article').filter({ hasText: 'Status:' });
  await expect(statusRow.locator('.property-value')).toHaveText('closed');
});
