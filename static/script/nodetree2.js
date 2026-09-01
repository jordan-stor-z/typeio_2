(() => {
  const { nodes, links } = JSON.parse(
    document.getElementById("graph-data").textContent
  );
  const svg = d3.select("svg");
  const zoomGroup = svg.select(".zoom-group");
  const zoom = d3.zoom()
    .scaleExtent([0.1, 3])
    .on("zoom", (event) => {
      zoomGroup.attr("transform", event.transform);
    });
  svg.call(zoom);
  // clientWidth/clientHeight report the SVG element's own box, but a
  // page header above it can push that box partly below the actual
  // viewport without shrinking it -- fitting to the box alone would
  // then still clip the bottom of a tall graph. Cap to what's actually
  // visible from the SVG's position to the edge of the window.
  const svgRect = svg.node().getBoundingClientRect();
  const svgWidth = Math.min(svg.node().clientWidth, window.innerWidth - svgRect.left);
  const svgHeight = Math.min(svg.node().clientHeight, window.innerHeight - svgRect.top);

  // --- Radial tidy tree, project root as the nucleus -----------------
  //
  // Two properties this layout is built to guarantee, in order:
  //
  //   1. No crossing edges. Every subtree owns a *disjoint* angular
  //      wedge, and a node is always placed strictly inside its
  //      parent's wedge. Two edges from different subtrees therefore
  //      live in non-overlapping wedges and cannot cross, and within a
  //      subtree the same argument recurses. (An earlier revision
  //      rotated each lone child by a fixed "spiral" step to make long
  //      chains curl; that shifts a child outside its parent's wedge
  //      into a sibling's, which is exactly what reintroduces
  //      crossings. Compact labels made the spiral unnecessary, so it's
  //      gone.)
  //   2. Compactness. Node radius is the *node's* size, not its
  //      label's: labels are wrapped to the node server-side
  //      (Data.Text.Util.wrapLabel), so a node occupies ~55px rather
  //      than the ~205px an unwrapped title used to demand. Ring
  //      spacing and wedge widths both derive from that, which is what
  //      keeps the whole graph inside a screenful instead of sprawling.
  //
  // Non-tree edges (a node depended on by more than one other -- a
  // shared dependency) are drawn on top of this tree as gentle curves;
  // they're the only edges that can cross anything, and there are few.
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const root = nodes.find((n) => n.nodeType === "project_root") ?? nodes[0];

  const neighbors = new Map(nodes.map((n) => [n.id, []]));
  links.forEach((l) => {
    neighbors.get(l.source).push(l.target);
    neighbors.get(l.target).push(l.source);
  });

  // BFS spanning tree: depth is hop count from root (direction of a
  // dependency edge is irrelevant to "how far from the nucleus"), and
  // the first node to reach a given node becomes its tree parent.
  const depth = new Map([[root.id, 0]]);
  const children = new Map(nodes.map((n) => [n.id, []]));
  const treeEdge = new Set();
  const queue = [root.id];
  while (queue.length) {
    const id = queue.shift();
    for (const nb of neighbors.get(id)) {
      if (depth.has(nb)) continue;
      depth.set(nb, depth.get(id) + 1);
      children.get(id).push(nb);
      treeEdge.add(`${id}:${nb}`);
      treeEdge.add(`${nb}:${id}`);
      queue.push(nb);
    }
  }
  // A node BFS never reaches (disconnected from root) still gets
  // attached under root so it renders somewhere sane instead of at a
  // NaN position -- shouldn't happen for a project's own dependency
  // graph, but the data isn't this script's to trust blindly.
  nodes.forEach((n) => {
    if (!depth.has(n.id)) {
      depth.set(n.id, 1);
      children.get(root.id).push(n.id);
    }
  });

  // Every node is the same size now that labels wrap to the node, so
  // one constant covers the circle plus a little breathing room.
  const nodeRadius = 52;
  const ringSpacing = nodeRadius * 2 + 44;
  // The drawn circle's own radius (CSS `#tree-container .node circle`),
  // as opposed to `nodeRadius` above, which is the clearance the layout
  // reserves around a node. Edges are trimmed back to this so they stop
  // at the circle's edge rather than running to its centre -- which is
  // what makes the arrowheads visible instead of buried under the node.
  const circleRadius = 45;
  const radiusAt = (d) => d * ringSpacing;

  // Angular width a subtree needs, computed bottom-up. A node needs
  // enough arc at its own radius to not touch its siblings; a parent
  // needs at least the sum of what its children need. Deeper rings are
  // physically longer, so the same clearance costs less angle out
  // there -- which is what stops a deep tree from fanning out into a
  // huge wasted circle the way a flat "share of 2*PI per leaf"
  // allocation does. Memoised: `place` below asks for the same
  // subtree's need once per sibling, which is exponential unmemoised.
  const needCache = new Map();
  const needOf = (id) => {
    if (needCache.has(id)) return needCache.get(id);
    const r = Math.max(radiusAt(depth.get(id)), ringSpacing);
    const own = (nodeRadius * 2.2) / r;
    const kids = children.get(id);
    const need = kids.length === 0
      ? own
      : Math.max(own, kids.reduce((sum, k) => sum + needOf(k), 0));
    needCache.set(id, need);
    return need;
  };

  // A ring only has so much room: n nodes on the ring at depth d need
  // n node-widths of arc, and a ring of radius d*ringSpacing only
  // offers 2*PI*d*ringSpacing of it. A wide project (many work nodes
  // hanging directly off the root) therefore doesn't fit on its ring at
  // the default spacing, and packing it in anyway is how nodes end up
  // overlapping. Since every node's angular need is inversely
  // proportional to its radius, pushing every ring out by exactly the
  // factor by which demand overflows a full turn makes it fit in one
  // shot -- no iteration, no guessing.
  const radiusScale = Math.max(1, needOf(root.id) / (2 * Math.PI));
  const scaledRadiusAt = (d) => radiusAt(d) * radiusScale;

  // Hand each subtree its own slice of its parent's wedge, sized by
  // that need, and place the node itself at the middle of its slice.
  const angle = new Map();
  const place = (id, start, end) => {
    angle.set(id, (start + end) / 2);
    const kids = children.get(id);
    if (kids.length === 0) return;
    const total = kids.reduce((sum, k) => sum + needOf(k), 0) || 1;
    let a = start;
    for (const k of kids) {
      const span = (end - start) * (needOf(k) / total);
      place(k, a, a + span);
      a += span;
    }
  };
  // The tree only claims as much of the circle as it actually needs
  // (capped at a full turn), rather than always fanning across 2*PI --
  // a two-branch project shouldn't be flung to opposite sides of the
  // screen just because there's room. Centred on -PI/2 so the graph
  // grows upward-ish from the nucleus rather than starting rightward.
  const sweep = Math.min(2 * Math.PI, needOf(root.id) / radiusScale);
  place(root.id, -Math.PI / 2 - sweep / 2, -Math.PI / 2 + sweep / 2);

  const cx = svgWidth / 2;
  const cy = svgHeight / 2;
  nodes.forEach((n) => {
    const r = scaledRadiusAt(depth.get(n.id));
    n.x = cx + r * Math.cos(angle.get(n.id));
    n.y = cy + r * Math.sin(angle.get(n.id));
  });

  // Fit the initial view to the layout just computed instead of a
  // fixed guess, so a small project isn't over-zoomed and a deep one
  // isn't mostly off-screen.
  const xs = nodes.map((n) => n.x);
  const ys = nodes.map((n) => n.y);
  const margin = nodeRadius + 30;
  const minX = Math.min(...xs) - margin, maxX = Math.max(...xs) + margin;
  const minY = Math.min(...ys) - margin, maxY = Math.max(...ys) + margin;
  const initialScale = Math.min(1.3, svgWidth / (maxX - minX), svgHeight / (maxY - minY));
  svg.call(
    zoom.transform,
    d3.zoomIdentity
      .translate(svgWidth / 2, svgHeight / 2)
      .scale(initialScale)
      .translate(-(minX + maxX) / 2, -(minY + maxY) / 2)
  );

  links.forEach((l) => {
    l.source = byId.get(l.source);
    l.target = byId.get(l.target);
    l.isTreeEdge = treeEdge.has(`${l.source.id}:${l.target.id}`);
  });

  // Every edge is drawn the same way -- a straight line, trimmed to the
  // node circles at both ends. A shared dependency is an ordinary
  // dependency and reads better looking like one; an earlier revision
  // bowed those aside and dashed them to mark them out as the edges
  // that can cross, but that's a distinction the reader didn't ask for
  // and mostly just added noise.
  const edgePath = (d) => {
    const dx = d.target.x - d.source.x, dy = d.target.y - d.source.y;
    const dist = Math.sqrt(dx * dx + dy * dy) || 1;
    // Stop the edge at each node's circle rather than its centre, so
    // the line doesn't disappear under the node and, more importantly,
    // so the arrowhead marking which way the dependency points stays
    // visible outside it.
    const trim = circleRadius + 4;
    const ux = dx / dist, uy = dy / dist;
    const x1 = d.source.x + ux * trim, y1 = d.source.y + uy * trim;
    const x2 = d.target.x - ux * trim, y2 = d.target.y - uy * trim;
    return `M${x1},${y1} L${x2},${y2}`;
  };

  svg.select("#graph-links")
    .selectAll("path.link")
    .data(links)
    .attr("class", (d) => (d.isTreeEdge ? "link" : "link link-shared"))
    .attr("d", edgePath);
  svg.select("#graph-nodes")
    .selectAll("g.node")
    .data(nodes)
    .attr("transform", (d) => `translate(${d.x},${d.y})`);

  // The layout is computed, not simulated -- there's nothing to settle
  // and nothing to wait for, so reveal it immediately. (The old
  // free-floating force simulation faded in on its "end" event, which
  // is why this used to be deferred.)
  svg.transition()
    .duration(300)
    .style("opacity", 1);
})();
