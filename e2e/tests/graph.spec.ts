import { test, expect } from '@playwright/test';
import { addNode, createProject } from './helpers';

// Fourth and last workflow covered by this suite (#97, follow-up to
// #94-#96): view the D3-rendered dependency graph, click a node, and
// confirm its detail panel opens with the `.node-highlight` glow, then
// that closing the panel clears it again. The `.flash` background-poll
// effect (Node.Refresh) is explicitly out of scope here, same as the
// proposal's hazards call out -- a one-shot transient, not this spec's
// concern.
//
// Clicking the node uses locator.dispatchEvent('click'), not
// locator.click(): found a serious, separate bug while writing this --
// the graph's D3 script (static/script/nodetree2.js) never positions
// any node past the first one in the list (confirmed via
// getAttribute('transform') returning null on the second node,
// regardless of how long you wait -- not a settle-timing issue, the
// simulation genuinely never touches that element). Filed as #120,
// with the root cause and fix; not fixed here, since this ticket is
// about the click/highlight behavior, not the graph's layout
// algorithm. dispatchEvent() fires the same hx-trigger="click" handler
// a real pointer click would, independent of where the (currently
// broken) layout happens to have put the element on screen -- once
// #120 lands, this can likely go back to a real click() against a
// predictably on-screen node.
test("clicking a graph node opens its detail panel and highlights it, closing clears both", async ({ page, request }) => {
  const project = await createProject(page, 'E2E graph project');
  const node = await addNode(request, project.id, 'E2E graph node');

  await page.goto(`/ui/project/vw?projectId=${project.id}`);

  // Settled-state check per the proposal's hazards: D3 renders after
  // data arrives, not synchronously with navigation -- assert on the
  // rendered SVG structure (both nodes present as real elements) before
  // interacting with either, rather than assuming the graph is ready
  // right after goto().
  await expect(page.locator('#graph-nodes .node')).toHaveCount(2);

  const graphNode = page.locator('#graph-nodes .node').filter({ hasText: node.title });
  await expect(graphNode).toBeAttached();
  await expect(graphNode).not.toHaveClass(/node-highlight/);

  await graphNode.dispatchEvent('click');

  // Opens #node-panel (Node.templateNodePanel), which itself loads the
  // plain node-detail view into #node-detail -- assert on that settled
  // content, not the panel's mere presence, so this also confirms the
  // click targeted the right node.
  await expect(page.locator('#node-detail header h2')).toHaveText(node.title);

  // The highlight is hyperscript-driven, tied to the panel element's
  // own htmx lifecycle (`init add .node-highlight to #node-<id> on
  // htmx:beforeCleanupElement remove .node-highlight from #node-<id>`),
  // not server state -- asserting on the graph node's class here, not
  // anything panel-side.
  await expect(graphNode).toHaveClass(/node-highlight/);

  // Close the panel (a normal button in #node-panel's own
  // panel-actions, unaffected by #120 -- no positioning issue here) and
  // confirm both the panel and the highlight clear.
  await page.getByRole('button', { name: 'close' }).click();
  await expect(page.locator('#node-panel')).toBeEmpty();
  await expect(graphNode).not.toHaveClass(/node-highlight/);
});
