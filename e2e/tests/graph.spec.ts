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
// Two things here changed with the radial layout (#162):
//
//   - Nodes are located by id (`#node-<id>`), not by their label text.
//     Labels now wrap to the node and truncate past three lines
//     (Data.Text.Util.wrapLabel), so a node's full title is no longer
//     present as one contiguous string to filter on -- and the id was
//     always the more robust handle anyway, being independent of how
//     the label happens to render.
//   - This uses a real click() again, not dispatchEvent('click'). The
//     workaround existed because of #120 (nodes never positioned, so
//     there was no reliable on-screen point to click); with the layout
//     now deterministic and fitted to the viewport, a real pointer
//     click works -- which also makes this a regression test for nodes
//     actually landing somewhere visible and clickable.
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

  const graphNode = page.locator(`#node-${node.id}`);
  await expect(graphNode).toBeAttached();
  await expect(graphNode).not.toHaveClass(/node-highlight/);

  await graphNode.click();

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

  // Close the panel and confirm both the panel and the highlight clear.
  await page.getByRole('button', { name: 'close' }).click();
  await expect(page.locator('#node-panel')).toBeEmpty();
  await expect(graphNode).not.toHaveClass(/node-highlight/);
});

// The layout's own contract (#162): the graph must render compactly and
// legibly, not sprawl or pile nodes on top of each other. Asserting on
// geometry here rather than eyeballing a screenshot -- this is the
// property that regressed twice while building the layout.
test("the dependency graph lays nodes out on-screen without overlapping them", async ({ page, request }) => {
  const project = await createProject(page, 'E2E graph layout');
  await addNode(request, project.id, 'E2E layout node A');
  await addNode(request, project.id, 'E2E layout node B');
  await addNode(request, project.id, 'E2E layout node C');

  await page.goto(`/ui/project/vw?projectId=${project.id}`);
  await expect(page.locator('#graph-nodes .node')).toHaveCount(4);

  const positions = await page.locator('#graph-nodes .node').evaluateAll((els) =>
    els.map((el) => {
      const m = (el.getAttribute('transform') || '').match(/translate\(([-\d.eE]+),([-\d.eE]+)\)/);
      return { id: el.id, x: m ? parseFloat(m[1]) : NaN, y: m ? parseFloat(m[2]) : NaN };
    })
  );

  // Every node positioned at all -- #120's regression, where every node
  // past the first kept a null transform.
  for (const p of positions) {
    expect(Number.isFinite(p.x) && Number.isFinite(p.y), `${p.id} has a real position`).toBe(true);
  }

  // And no two of them stacked on top of each other: node circles are
  // r=45 (manage-project.css), so centres must be at least a diameter
  // apart to not overlap.
  for (let i = 0; i < positions.length; i++) {
    for (let j = i + 1; j < positions.length; j++) {
      const a = positions[i], b = positions[j];
      const dist = Math.hypot(a.x - b.x, a.y - b.y);
      expect(dist, `${a.id} and ${b.id} do not overlap`).toBeGreaterThanOrEqual(90);
    }
  }
});
