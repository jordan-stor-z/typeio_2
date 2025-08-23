(() => {
  const { nodes, links } = JSON.parse(
    document.getElementById("graph-data").textContent
  );
  const svg = d3.select("svg");
  const zoomGroup = svg.select(".zoom-group");
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
    .alphaDecay(0.09)
    .velocityDecay(0.5);
  const link = svg.select("#graph-links")
    .selectAll("line")
    .data(links);
  const node = svg.select("#graph-nodes")
    .selectAll("g")
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
