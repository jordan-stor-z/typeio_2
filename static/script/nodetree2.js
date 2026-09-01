(() => {
  const { nodes, links } = JSON.parse(
    document.getElementById("graph-data").textContent
  );
  const svg = d3.select("svg");
  const zoomGroup = svg.select(".zoom-group");
  // Lower bound is generous (0.05) because footprint-based spacing
  // below can make the graph's real extent much bigger than a fixed
  // ringSpacing ever produced -- a long, deep dependency chain needs to
  // still be zoomable out far enough to see end to end.
  const zoom = d3.zoom()
    .scaleExtent([0.05, 3])
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

  // --- Radial layout: the project root is the nucleus ----------------
  // Every work node's distance from root follows its dependency depth
  // (BFS hop count over the links, direction ignored -- "how far from
  // root" doesn't care which way a dependency arrow points), not
  // wherever a generic charge/link/center force simulation happens to
  // settle it. Nodes sharing a branch also get angularly grouped (see
  // `place` below), which is what keeps dependency lines from crossing.
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const root = nodes.find((n) => n.nodeType === "project_root") ?? nodes[0];

  const neighbors = new Map(nodes.map((n) => [n.id, []]));
  links.forEach((l) => {
    neighbors.get(l.source).push(l.target);
    neighbors.get(l.target).push(l.source);
  });

  const children = new Map(nodes.map((n) => [n.id, []]));
  const visited = new Set([root.id]);
  const queue = [root.id];
  while (queue.length) {
    const id = queue.shift();
    for (const nb of neighbors.get(id)) {
      if (visited.has(nb)) continue;
      visited.add(nb);
      children.get(id).push(nb);
      queue.push(nb);
    }
  }
  // A node BFS never reaches (disconnected from root) still gets
  // attached under root so it renders somewhere sane instead of at a
  // NaN position -- shouldn't happen for a project's own dependency
  // graph, but the data isn't this script's to trust blindly.
  nodes.forEach((n) => {
    if (!visited.has(n.id)) children.get(root.id).push(n.id);
  });

  // Give each node an angular slot sized by its own subtree, so a whole
  // dependency branch stays grouped together around its ring instead of
  // scattering wherever physics puts it -- this is what keeps
  // dependency lines from crossing.
  const subtreeSize = new Map();
  const sizeOf = (id) => {
    if (subtreeSize.has(id)) return subtreeSize.get(id);
    const kids = children.get(id);
    const size = kids.length === 0 ? 1 : kids.reduce((sum, k) => sum + sizeOf(k), 0);
    subtreeSize.set(id, size);
    return size;
  };
  nodes.forEach((n) => sizeOf(n.id));

  const angle = new Map([[root.id, 0]]);
  const place = (id, start, end) => {
    angle.set(id, (start + end) / 2);
    let a = start;
    for (const k of children.get(id)) {
      const span = (end - start) * (sizeOf(k) / sizeOf(id));
      place(k, a, a + span);
      a += span;
    }
  };
  place(root.id, 0, 2 * Math.PI);

  // Node labels are long, single-line, and don't wrap (see the `text`
  // element `nodeContents` renders), so the circle's own 45px CSS
  // radius understates how much room a node actually needs on screen --
  // a title like "Final Inspection & Occupancy Certification" is far
  // wider than its circle. `footprint` estimates that on-screen half-
  // width so spacing can account for it.
  const footprint = (n) => 45 + Math.min(160, n.label.length * 3.2);

  // Radius grows outward from root by accumulating each node's own and
  // its parent's footprint along its branch (plus a fixed gap), rather
  // than a flat depth * constant -- so a long-titled node automatically
  // gets more room than a short one, instead of leaning on collision
  // force to shove overlapping labels apart after the fact.
  const gap = 30;
  const radius = new Map([[root.id, 0]]);
  const placeRadius = (id) => {
    for (const k of children.get(id)) {
      radius.set(k, radius.get(id) + footprint(byId.get(id)) + footprint(byId.get(k)) + gap);
      placeRadius(k);
    }
  };
  placeRadius(root.id);

  const cx = svgWidth / 2;
  const cy = svgHeight / 2;
  nodes.forEach((n) => {
    const r = radius.get(n.id);
    n.targetX = cx + r * Math.cos(angle.get(n.id));
    n.targetY = cy + r * Math.sin(angle.get(n.id));
    // Seed the real position from the target too, so the simulation
    // below starts from the intended layout and only has to settle
    // minor cross-branch crowding, rather than discover the layout
    // from scratch the way the old free-floating charge/link/center
    // setup did.
    n.x = n.targetX;
    n.y = n.targetY;
  });
  // The root is the nucleus -- pin it exactly at the SVG's true center
  // (fx/fy make a node immune to every force) so it always renders
  // there deterministically, regardless of what happens elsewhere.
  root.fx = cx;
  root.fy = cy;

  // Fit the initial view to the layout just computed instead of a
  // fixed guess -- footprint-based spacing means the graph's real size
  // varies a lot with dependency depth and title length, so a constant
  // initial zoom (the old code used a flat 1.3x) would leave a small
  // project too zoomed in and a deep one mostly off-screen.
  const extentX = nodes.map((n) => [n.targetX - footprint(n), n.targetX + footprint(n)]).flat();
  const extentY = nodes.map((n) => [n.targetY - footprint(n), n.targetY + footprint(n)]).flat();
  const margin = 100;
  const spanX = Math.max(...extentX) - Math.min(...extentX) + margin * 2;
  const spanY = Math.max(...extentY) - Math.min(...extentY) + margin * 2;
  const midX = (Math.max(...extentX) + Math.min(...extentX)) / 2;
  const midY = (Math.max(...extentY) + Math.min(...extentY)) / 2;
  const initialScale = Math.min(1.3, svgWidth / spanX, svgHeight / spanY);
  svg.call(
    zoom.transform,
    d3.zoomIdentity
      .translate(svgWidth / 2, svgHeight / 2)
      .scale(initialScale)
      .translate(-midX, -midY)
  );

  links.forEach((l) => {
    l.source = byId.get(l.source);
    l.target = byId.get(l.target);
  });

  // A short, tightly-anchored simulation exists only to settle any
  // remaining cross-branch crowding (forceCollide) around the layout
  // just computed -- not to discover the layout itself, which is why
  // it's anchored back to each node's own target position/radius
  // rather than left to freely wander.
  const simulation = d3.forceSimulation(nodes)
    .force("radial", d3.forceRadial((d) => radius.get(d.id), cx, cy).strength(0.6))
    .force("x", d3.forceX((d) => d.targetX).strength(0.4))
    .force("y", d3.forceY((d) => d.targetY).strength(0.4))
    .force("collide", d3.forceCollide(footprint))
    .alphaDecay(0.06)
    .velocityDecay(0.6);
  const link = svg.select("#graph-links")
    .selectAll("line")
    .data(links);
  const node = svg.select("#graph-nodes")
    .selectAll("g.node")
    .data(nodes);
  simulation.on("tick", () => {
    link
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);
    node.attr("transform", d => `translate(${d.x},${d.y})`);
  });
  simulation.on("end", () => {
      svg.transition()
        .duration(500)
        .style("opacity", 1);
  });
})();
