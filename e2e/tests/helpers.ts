import { APIRequestContext, Page, expect } from '@playwright/test';

// Shared setup helpers for this suite's specs -- reused rather than
// duplicated across specs, per each ticket's "reuse #94's Playwright
// setup" call.

export interface CreatedProject {
  id: string;
  title: string;
  description: string;
}

// Drives the add-project UI form end to end and returns the resulting
// project's id (scraped from its rendered card -- ProjectIndex.List's
// `.id` span -- since project creation has no JSON API to read the id
// back from directly). This is the same flow create-project.spec.ts
// exercises as its own test; other specs that just need *a* project to
// exist call this instead of reimplementing it.
export async function createProject(page: Page, titlePrefix: string): Promise<CreatedProject> {
  const title = `${titlePrefix} ${Date.now()}`;
  const description = `Created by e2e/tests/helpers.ts's createProject() at ${new Date().toISOString()}`;

  await page.goto('/ui/projects/vw');
  await page.getByRole('button', { name: 'Create Project' }).click();

  // Not page.getByLabel(...): the add-project form's <label for="...">
  // doesn't reference a matching input id (Domain.Project.Responder.Ui.ProjectCreate.View
  // only sets `name`, not `id`), so the label/control association
  // getByLabel depends on doesn't hold. Scoping by `name` instead, which
  // does match the responder's actual form-decoding key
  // (Submit.paramForm).
  await page.locator('input[name="title"]').fill(title);
  await page.locator('textarea[name="description"]').fill(description);
  await page.getByRole('button', { name: 'Submit' }).click();

  // Not page.locator('#project-index').filter(...): #project-index is
  // the single list container (one match, so filter() has nothing to
  // narrow among) -- #project-item is the per-card div. It's also a
  // non-unique id (ProjectIndex.List renders one per card, all sharing
  // the same literal id -- invalid HTML, a real finding, not fixed
  // here), but that's exactly what makes filter() work correctly here:
  // it resolves to every card, then narrows to the one containing this
  // title.
  const card = page.locator('#project-item').filter({ hasText: title });
  await expect(card.getByRole('heading', { name: title, level: 3 })).toBeVisible();

  const id = await card.locator('.id').innerText();
  return { id: id.trim(), title, description };
}

export interface CreatedNode {
  id: string;
  title: string;
  description: string;
  projectId: string;
}

// Adds a node to an existing project via a direct API call
// (Domain.Project.Responder.Api.Node.Post), not a UI interaction: the
// app has no UI affordance to create a node anywhere -- checked the D3
// script, the graph template, and the node panel (see #95's PR
// description for the full finding). Every spec that needs *a* node to
// exist but isn't testing node creation itself calls this instead of
// reimplementing the API-plus-lookup dance.
export async function addNode(
  request: APIRequestContext,
  projectId: string,
  titlePrefix: string
): Promise<CreatedNode> {
  const title = `${titlePrefix} ${Date.now()}`;
  const description = `Created by e2e/tests/helpers.ts's addNode() at ${new Date().toISOString()}`;

  const created = await request.post('/api/project/nodes', {
    form: { title, description, projectId },
  });
  if (!created.ok()) {
    throw new Error(`addNode: POST /api/project/nodes failed: ${created.status()} ${await created.text()}`);
  }

  // The POST response above is just "Ok" -- no created-node id -- so
  // fetch it back to find the id. Domain.Project.Responder.Api.Node.Get
  // returns every node in the database, unfiltered by project (#114,
  // filed as a follow-up, not fixed here); the timestamped title is
  // what actually picks out the right one.
  const allNodes = await request.get('/api/project/nodes').then(r => r.json());
  const node = allNodes.find((n: { title: string }) => n.title === title);
  if (!node) {
    throw new Error(`addNode: no node titled ${JSON.stringify(title)} in ${JSON.stringify(allNodes)}`);
  }

  return { id: String(node.nodeId), title, description, projectId };
}
