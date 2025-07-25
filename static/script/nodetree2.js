function myGraph(el) {
  this.el = el;
  this.addNode = function(node, links = []) {
    this.nodes.push(node);
    this.links.concat(links);
    this.simulation.nodes(this.nodes);
    this.simulation.force("link").links(this.links);
    this.simulation.alpha(0.3).restart();
    this.linkElements = this.linkEl.selectAll("line")
      .data(this.links, d => `${d.source.id}-${d.target.id}`)
      .join("line")
      .attr("class", "link")
      .attr("stroke", "#999")
      .attr("stroke-opacity", 0.6)
      .attr("stroke-width", 2)
      .attr("marker-end", "url(#arrow)");
    this.nodeElements = this.nodeEl.selectAll("g")
      .data(this.nodes, d => d.id)
      .join(
        enter => {
          const g = enter.append("g")
            .attr("class", "node")
            .attr("hx-trigger", "click")
            .attr("hx-target", "#node-panel")
            .attr("hx-swap", "innerHTML");

          g.append("circle")
            .attr("stroke", "white")
            .attr("stroke-width", 1.5);

          g.append("text")
            .attr("font-size", "10px")
            .attr("text-anchor", "middle")
            .attr("dy", "0.35em")
            .attr("fill", "white");

          return g;
        }
      )
      .attr("hx-get", d => d.link)
      .attr("hx-push-url", d => d.push);
    this.nodeElements.select("circle")
       .attr("class", d => d.pinned ? "root" : "work");
    this.nodeElements.select("text")
      .text(d => d.label);
  }

  this.initialize = function() {
    const { nodes: initNodes, links: initLinks } = JSON.parse(
        document.getElementById("graph-data").textContent
      );

    this.nodes = initNodes;
    this.links = initLinks;

    const svg = d3.select(el);

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

    this.zoomGroup = svg.append("g");

    const zoom = d3.zoom()
      .scaleExtent([0.5, 3])
      .on("zoom", (event) => {
        this.zoomGroup.attr("transform", event.transform);
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

    this.simulation = d3.forceSimulation(this.nodes)
      .force("link", d3.forceLink(this.links).id(d => d.id).distance(180))
      .force("charge", d3.forceManyBody().strength(-600))
      .force("center", d3.forceCenter(450, 325))
      .alphaDecay(0.09)    // Increase alphaDecay for faster convergence
      .velocityDecay(0.5); // Increase velocityDecay to dampen movement quicker

    this.linkEl = this.zoomGroup.append("g")
      .selectAll("line")
      .data(this.links)
      .join("line")
      .attr("class", "link")
      .attr("stroke", "#999")
      .attr("stroke-opacity", 0.6)
      .attr("stroke-width", 2)
      .attr("marker-end", "url(#arrow)");

    this.nodeEl = this.zoomGroup.append("g")
      .selectAll("g")
      .data(this.nodes)
      .join("g")
      .attr("class", "node")
      .attr("hx-get", d => d.link)
      .attr("hx-trigger", "click")
      .attr("hx-target", "#node-panel")
      .attr("hx-push-url", d => d.push)
      .attr("hx-swap", "innerHTML");

    this.nodeEl.append("circle")
      .attr("class", d => d.pinned ? "root" : "work")
      .attr("stroke", "white")
      .attr("stroke-width", 1.5);

    this.nodeEl.append("text")
      .text(d => d.label)
      .attr("font-size", "10px")
      .attr("text-anchor", "middle")
      .attr("dy", "0.35em")
      .attr("fill", "white");

    this.simulation.on("tick", () => {
      this.linkEl 
        .attr("x1", d => d.source.x)
        .attr("y1", d => d.source.y)
        .attr("x2", d => d.target.x)
        .attr("y2", d => d.target.y);

      this.nodeEl.attr("transform", d => `translate(${d.x},${d.y})`);
    });
  }
}
