import { Page, expect } from '@playwright/test';

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
