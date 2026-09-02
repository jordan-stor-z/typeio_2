import { test, expect } from '@playwright/test';
import { addNode, createProject } from './helpers';

// Fourth and last workflow covered by this suite (#97, follow-up to
// #94-#96): view the dependency graph, click a node, and
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

  // Settled-state check per the proposal's hazards: the graph arrives
  // by an htmx swap into #tree-container, not synchronously with
  // navigation -- assert on the rendered SVG structure (both nodes
  // present as real elements) before interacting with either, rather
  // than assuming the graph is ready right after goto(). Since #181 the
  // SVG is server-rendered, so there is no client layout pass to wait
  // on beyond that swap.
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

  // Boxes now, not circles (#178), and the server places them (#181):
  // the group's transform is the box's *top-left*, and its size is on
  // the rect. Reading both means this can assert real box overlap
  // rather than the centre-distance proxy the circle version used.
  const boxes = await page.locator('#graph-nodes .node').evaluateAll((els) =>
    els.map((el) => {
      const m = (el.getAttribute('transform') || '').match(/translate\(([-\d.eE]+),([-\d.eE]+)\)/);
      const rect = el.querySelector('rect');
      return {
        id: el.id,
        x: m ? parseFloat(m[1]) : NaN,
        y: m ? parseFloat(m[2]) : NaN,
        w: rect ? parseFloat(rect.getAttribute('width') || '') : NaN,
        h: rect ? parseFloat(rect.getAttribute('height') || '') : NaN,
      };
    })
  );

  // Every node positioned and sized at all -- #120's regression, where
  // every node past the first kept a null transform.
  for (const b of boxes) {
    expect(
      Number.isFinite(b.x) && Number.isFinite(b.y),
      `${b.id} has a real position`,
    ).toBe(true);
    expect(Number.isFinite(b.w) && Number.isFinite(b.h), `${b.id} has a real size`).toBe(true);
  }

  // And no two boxes overlapping, which for axis-aligned rectangles is
  // exact rather than a proxy: they overlap only if they overlap on
  // both axes at once.
  for (let i = 0; i < boxes.length; i++) {
    for (let j = i + 1; j < boxes.length; j++) {
      const a = boxes[i], b = boxes[j];
      const overlaps =
        a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h;
      expect(overlaps, `${a.id} and ${b.id} do not overlap`).toBe(false);
    }
  }
});

// The cutover (#181): the server-computed graph is what the app serves,
// with no query parameter. Before this, every one of #173-#180 was
// reachable only via `?layout=server`, which nothing in the UI set
// (#192) -- so this is the first test that drives any of it in a real
// browser.
test("the graph renders server-side, with no client layout script", async ({ page, request }) => {
  const project = await createProject(page, 'E2E server layout');
  await addNode(request, project.id, 'E2E server node');

  await page.goto(`/ui/project/vw?projectId=${project.id}`);
  await expect(page.locator('#graph-nodes .node')).toHaveCount(2);

  // Rounded boxes, classed by kind -- what manage-project.css styles.
  await expect(page.locator('#graph-nodes .node rect.root')).toHaveCount(1);
  await expect(page.locator('#graph-nodes .node rect.work')).toHaveCount(1);
  await expect(page.locator('#graph-nodes circle')).toHaveCount(0);

  // The graph no longer leaves the server as data for a client to lay
  // out, so there is nothing for one to read.
  await expect(page.locator('#graph-data')).toHaveCount(0);
});

// The viewport (#179), driven for the first time here for the same
// reason as above.
test("the graph viewport opens on the project root and zooms", async ({ page, request }) => {
  const project = await createProject(page, 'E2E viewport');
  for (const t of ['Viewport node A', 'Viewport node B', 'Viewport node C']) {
    await addNode(request, project.id, t);
  }

  await page.goto(`/ui/project/vw?projectId=${project.id}`);
  await expect(page.locator('#graph-nodes .node')).toHaveCount(4);

  const svg = page.locator('#tree-view');
  const widthOf = async () => parseFloat((await svg.getAttribute('width')) || '');

  // Opens at natural size -- deliberately not scaled to fit, which is
  // what would shrink titles past legibility on a big project.
  const base = parseFloat((await svg.getAttribute('data-base-width')) || '');
  expect(base).toBeGreaterThan(0);
  expect(await widthOf()).toBeCloseTo(base, 0);

  // Zoom in, then out, and confirm the SVG is resized rather than the
  // page merely scrolling.
  await page.locator('#graph-zoom-in').click();
  const zoomedIn = await widthOf();
  expect(zoomedIn).toBeGreaterThan(base);

  await page.locator('#graph-zoom-out').click();
  expect(await widthOf()).toBeLessThan(zoomedIn);

  // Recentre resets the scale outright.
  await page.locator('#graph-zoom-in').click();
  await page.locator('#graph-zoom-reset').click();
  expect(await widthOf()).toBeCloseTo(base, 0);
});

// Pointer-drag panning exists *because* the scrollbars are hidden
// (#179): a wheel-less mouse would otherwise have no way to pan. The
// hazard it introduces is that every node is also a click target, so
// this checks both halves -- the drag scrolls, and it does not open a
// node's panel on the way.
test("dragging the canvas pans it without opening a node", async ({ page, request }) => {
  const project = await createProject(page, 'E2E pan');
  for (const t of ['Pan node A', 'Pan node B', 'Pan node C', 'Pan node D']) {
    await addNode(request, project.id, t);
  }

  await page.goto(`/ui/project/vw?projectId=${project.id}`);
  await expect(page.locator('#graph-nodes .node')).toHaveCount(5);

  // Zoom in first so the drawing is comfortably larger than its
  // container and there is somewhere to scroll to.
  for (let i = 0; i < 3; i++) await page.locator('#graph-zoom-in').click();

  const container = page.locator('#tree-container');
  const before = await container.evaluate((el) => el.scrollLeft);

  const box = await container.boundingBox();
  if (!box) throw new Error('#tree-container has no box');
  const midY = box.y + box.height / 2;

  await page.mouse.move(box.x + box.width * 0.7, midY);
  await page.mouse.down();
  // Several small steps rather than one jump: a drag is a stream of
  // pointermove events, and one teleporting move is not what a real
  // pointer produces.
  for (let i = 1; i <= 5; i++) {
    await page.mouse.move(box.x + box.width * 0.7 - i * 30, midY);
  }
  await page.mouse.up();

  await expect
    .poll(() => container.evaluate((el) => el.scrollLeft))
    .toBeGreaterThan(before);

  // The drag must not have been read as a click on whatever was under
  // the pointer.
  await expect(page.locator('#node-panel')).toBeEmpty();
});
