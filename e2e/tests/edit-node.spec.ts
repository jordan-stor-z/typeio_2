import { test, expect } from '@playwright/test';
import { createProject } from './helpers';

// Second workflow covered by this suite (#95, follow-up to #94's
// create-project pilot): add a node, then edit its title and
// description via the node-detail panel.
//
// The "add a node" step here is a direct API call
// (Domain.Project.Responder.Api.Node.Post), not a UI interaction: the
// app currently has no UI affordance to create a node at all -- no
// button, form, or graph interaction posts to that endpoint anywhere
// (checked the D3 script, the graph template, and the node panel).
// Reusing that existing API as setup, the same way #94's own suite
// reuses `make seed-db` for reference data rather than reinventing
// seeding, keeps this spec focused on what's actually UI-testable here:
// editing. See the PR description for this finding -- it's a real gap
// worth its own ticket, not something to silently work around forever.
//
// Opening the node panel goes through the URL's `nodeId` query param
// (ProjectManage.View's own supported deep-link shape -- the same one
// Graph.pushUrl puts in the address bar on a real click) rather than
// clicking the node in the D3 graph. That's deliberate, not a
// workaround: the graph's force layout can settle a node at a
// genuinely off-screen position (observed a 2-node graph settle with
// one node's bounding box at x:-191, entirely under the page header) --
// that's real app behavior, not a test artifact, and interacting with
// the graph itself is #97's scope, not this ticket's. The direct-link
// path exercises a real, already-supported way into this same panel
// without depending on where the graph happens to lay a node out.
test('editing a node updates its title and description', async ({ page, request }) => {
  const project = await createProject(page, 'E2E edit-node project');

  const nodeTitle = `E2E edit-node ${Date.now()}`;
  const created = await request.post('/api/project/nodes', {
    form: {
      title: nodeTitle,
      description: 'Created directly via the API as this spec\'s fixture data.',
      projectId: project.id,
    },
  });
  expect(created.ok(), `${created.status()} ${await created.text()}`).toBeTruthy();

  // The POST response above is just "Ok" -- no created-node id -- so
  // fetch it back to find the id to deep-link to. Domain.Project.Responder.Api.Node.Get
  // returns every node in the database, unfiltered by project (a
  // separate finding, not this ticket's to fix); the timestamped title
  // is what actually picks out the right one here.
  const allNodes = await request.get('/api/project/nodes').then(r => r.json());
  const node = allNodes.find((n: { title: string }) => n.title === nodeTitle);
  expect(node, `no node titled ${JSON.stringify(nodeTitle)} in ${JSON.stringify(allNodes)}`).toBeTruthy();

  await page.goto(`/ui/project/vw?projectId=${project.id}&nodeId=${node.nodeId}`);

  // Opens the node panel (#node-panel), which itself loads the
  // non-editable detail view into #node-detail. Switch to the editable
  // form via the pencil-icon button (Node.templateNodePanel) -- its
  // accessible name is the Material Icons ligature text itself
  // ("mode_edit"), not a rendered glyph, so this is a real DOM text
  // match, not something that depends on how the icon font renders.
  await page.getByRole('button', { name: 'mode_edit' }).click();

  const newTitle = `${nodeTitle} (edited)`;
  const newDescription = `Edited by e2e/tests/edit-node.spec.ts at ${new Date().toISOString()}`;

  // #node-title has a real id (unlike the add-project form's inputs --
  // see create-project.spec.ts's note) but it still doesn't match its
  // <label for="title">, which points at nothing -- same underlying gap,
  // scoped by id/name instead of getByLabel() here too.
  //
  // selectText() + pressSequentially(), not fill(): confirmed by direct
  // testing that fill() never fires htmx's `input changed delay:500ms`
  // trigger on these fields at all -- no PUT request, ever, no matter
  // how long you wait. Real keystrokes (pressSequentially()) do fire
  // it, but only cleanly after selectText() (not fill('')) clears the
  // existing value first -- fill('') as a "clear" step suppresses the
  // same trigger fill() always does, even for the real keystrokes that
  // follow it. selectText() doesn't have that effect: it's a genuine
  // selection, not a value write.
  const title = page.locator('#node-title');
  await title.selectText();
  await title.pressSequentially(newTitle);
  const description = page.locator('#node-detail textarea[name="description"]');
  await description.selectText();
  await description.pressSequentially(newDescription);

  // Both fields debounce (`input changed delay:500ms`) before PUTting
  // and swapping their own indicator-box -- assert on the settled
  // success icon (a literal "done" ligature, Node.Title/Description's
  // templatePostSuccess/templatePutSuccess), never the mid-debounce
  // `.loading` spinner the proposal's hazards explicitly warn about.
  await expect(page.locator('label[for="title"] .indicator-box i.material-icons')).toHaveText('done');
  await expect(page.locator('label[for="description"] .indicator-box i.material-icons')).toHaveText('done');

  // Closing the edit view (the check-icon button, revealed once the
  // pencil's own load completes) re-fetches the plain node-detail view
  // from the database -- asserting on it here confirms the edits
  // actually persisted, not just that each indicator claimed success.
  await page.getByRole('button', { name: 'check' }).click();
  await expect(page.locator('#node-detail header h2')).toHaveText(newTitle);
  await expect(page.locator('#node-detail section p')).toHaveText(newDescription);
});
