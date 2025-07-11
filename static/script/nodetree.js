(() => {
  const { nodes, links } = JSON.parse(
      document.getElementById("graph-data").textContent
    );

  const svg = d3.select("svg");

  svg.append("defs").append("marker")
    .attr("id", "arrow")
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 30)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5")
    .attr("fill", "#999");

  const zoomGroup = svg.append("g");

  const zoom = d3.zoom()
    .scaleExtent([0.5, 3])
    .on("zoom", (event) => {
      zoomGroup.attr("transform", event.transform);
    });

  svg.call(zoom);

  const initialScale = 1.3;
  const svgWidth = svg.node().clientWidth;
  const svgHeight = svg.node().clientHeight;

  svg.call(
    zoom.transform,
    d3.zoomIdentity
      .translate(svgWidth * (1 - initialScale) / 2, svgHeight * (1 - initialScale) / 2)
      .scale(initialScale)
  );

  const simulation = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(links).id(d => d.id).distance(180))
    .force("charge", d3.forceManyBody().strength(-600))
    .force("center", d3.forceCenter(450, 325))
    .alphaDecay(0.09)    // Increase alphaDecay for faster convergence
    .velocityDecay(0.5); // Increase velocityDecay to dampen movement quicker

  const link = zoomGroup.append("g")
    .selectAll("line")
    .data(links)
    .join("line")
    .attr("class", "link")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.6)
    .attr("stroke-width", 2)
    .attr("marker-end", "url(#arrow)");

  const node = zoomGroup.append("g")
    .selectAll("g")
    .data(nodes)
    .join("g")
    .attr("class", "node")
    .attr("hx-get", d => d.link)
    .attr("hx-trigger", "click")
    .attr("hx-target", "#node-detail")
    .attr("hx-swap", "innerHTML");

  node.append("circle")
    .attr("class", d => d.pinned ? "root" : "work")
    .attr("stroke", "white")
    .attr("stroke-width", 1.5);

  node.append("text")
    .text(d => d.label)
    .attr("font-size", "10px")
    .attr("text-anchor", "middle")
    .attr("dy", "0.35em")
    .attr("fill", "white");

  simulation.on("tick", () => {
    link
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);

    node.attr("transform", d => `translate(${d.x},${d.y})`);
  });
})();
