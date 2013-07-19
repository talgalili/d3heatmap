function heatmap(selector, data) {
  var el = d3.select(selector);
  
  (function() {
    var inner = el.append("div").classed("inner", true);
    var info = inner.append("div").classed("info", true);
    var colDend = inner.append("svg").classed("colDend", true);
    var rowDend = inner.append("svg").classed("rowDend", true);
    var colmap = inner.append("svg").classed("colormap", true);
  })();
  
  var xZoomBehavior = d3.behavior.zoom().scaleExtent([1, Infinity]);
  var yZoomBehavior = d3.behavior.zoom().scaleExtent([1, Infinity]);
  var colormapZooms = [
    d3.behavior.zoom().scaleExtent([1, Infinity]),
    d3.behavior.zoom().scaleExtent([1, Infinity])
    ];
  
  el.select('.colDend').call(xZoomBehavior);
  el.select('.rowDend').call(yZoomBehavior);
  
  var row = dendrogram(el.select('svg.rowDend'), data.rows, false, 250, 500, yZoomBehavior);
  var col = dendrogram(el.select('svg.colDend'), data.cols, true, 600, 250, xZoomBehavior);
  var colormap = colormap(el.select('svg.colormap'), data.matrix, 600, 500, colormapZooms);
  
  function updateColormapZoom() {
    var xZoom = colormapZooms[0];
    var yZoom = colormapZooms[1];
    xZoom.scale(xZoomBehavior.scale());
    yZoom.scale(yZoomBehavior.scale());
    xZoom.translate(xZoomBehavior.translate());
    yZoom.translate(yZoomBehavior.translate());
    colormap.draw();
  }
  
  xZoomBehavior.on('zoom', function() {
    col.draw();
    updateColormapZoom();
  });
  yZoomBehavior.on('zoom', function() {
    row.draw();
    updateColormapZoom();
  });
  
  function colormap(svg, data, width, height, zoomBehaviors) {
    // Check for no data
    if (data.length === 0)
      return function() {};
    
    var cols = data.dim[1]
    var rows = data.dim[0];
    
    var merged = data.data;
    
    var x = d3.scale.linear().domain([0, cols]).range([0, width]);
    var y = d3.scale.linear().domain([0, rows]).range([0, height]);
    var color = d3.scale.linear()
    .domain(data.domain)
    .range(data.colors);
    
    zoomBehaviors[0].x(x);
    zoomBehaviors[1].y(y);
    
    svg = svg
    .attr("width", width)
    .attr("height", height)
    .append("g");
    var rect = svg.selectAll("rect").data(merged);
    rect.enter().append("rect").classed("datapt", true);
    rect.exit().remove();
    rect
    .property("colIndex", function(d, i) { return i % cols; })
    .property("rowIndex", function(d, i) { return Math.floor(i / cols); })
    .attr("x", function(d, i) {
      return x(i % cols);
    })
    .attr("y", function(d, i) {
      return y(Math.floor(i / cols));
    })
    .attr("width", x(1))
    .attr("height", y(1))
    .attr("fill", function(d) { return color(d); })
    .append("title").text(function(d) { return d + ""; });
    
    function draw() {
      var t = [zoomBehaviors[0].translate()[0], zoomBehaviors[1].translate()[1]];
      var s = [zoomBehaviors[0].scale(), zoomBehaviors[1].scale()];
      svg
      .attr("transform", "translate(" + t[0] + " " + t[1] + ") " +
              "scale(" + s[0] + " " + s[1] + ")");
    }
    draw();
    
    return {
      draw: draw
    };
  }
  
  function dendrogram(svg, data, rotated, width, height, zoomBehavior) {
    
    var x = d3.scale.linear();
    var y = d3.scale.linear()
    .domain([0, height])
    .range([0, height]);
    
    var cluster = d3.layout.cluster()
    .separation(function(a, b) { return 1; })
    .size([rotated ? width : height, (rotated ? height : width) - 160]);
    
    var transform = "translate(40,0)";
    if (rotated) {
      // Flip dendrogram vertically
      x.range([1, 0]);
      // Rotate
      transform = "rotate(-90," + height/2 + "," + height/2 + ") translate(140, 0)";
    }
    
    if (rotated)
      zoomBehavior.x(y);
    else
      zoomBehavior.y(y);
    
    svg = svg
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", transform);
    
    var nodes = cluster.nodes(data),
    links = cluster.links(nodes);
    
    function draw() {
      // Constrain translation to extent
      if (d3.event) {
        var t = d3.event.translate;
        var s = d3.event.scale;
        if (rotated)
          t[0] = Math.max(-width * (s - 1), Math.min(0, t[0]));
        else
          t[1] = Math.max(-height * (s - 1), Math.min(0, t[1]));
        zoomBehavior.translate(t);
      }
      
      function elbow(d, i) {
        return x(d.source.y) + "," + y(d.source.x) + " " +
          x(d.source.y) + "," + y(d.target.x) + " " +
          x(d.target.y) + "," + y(d.target.x);
      }
      
      var link = svg.selectAll(".link")
      .data(links)
      .attr("points", elbow)
      .enter().append("polyline")
      .attr("class", "link")
      .attr("points", elbow);
      
      var node = svg.selectAll(".node")
      .data(nodes)
      .attr("transform", function(d) { return "translate(" + x(d.y) + "," + y(d.x) + ")"; })
      .enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + x(d.y) + "," + y(d.x) + ")"; });
      
      var anchor = rotated ? "end" : "start";
      var dx = rotated ? -3 : 3;
      var leafNode = node.filter(function(d, i){ return !d.children; })
      .append("text")
      .attr("dx", dx)
      .attr("dy", 3)
      .style("text-anchor", anchor)
      .text(function(d) { return d.name; });
      
      return leafNode;
    }
    var leaves = draw();
    return {
      draw: draw,
      leaves: leaves[0]
    };
  }
  
  var dispatcher = d3.dispatch('hover', 'click');
  
  $('.datapt').on('mouseover', function() {
    $('.info').text($(this).children('title').text());
    d3.select(row.leaves[this.rowIndex]).classed('active', true);
    d3.select(col.leaves[this.colIndex]).classed('active', true);
    dispatcher.hover({
      data: {
        value: +$(this).children('title').text(),
        row: this.rowIndex,
        col: this.colIndex
      }
    });
  });
  $('.datapt').on('click', function() {
    $('.info').text($(this).children('title').text());
    d3.selectAll('.datapt.clicked').classed('clicked', false);
    d3.select(row.leaves[this.rowIndex]).classed('clicked', true);
    d3.select(this).classed('clicked', true);
    dispatcher.click({
      data: {
        value: +$(this).children('title').text(),
        row: this.rowIndex,
        col: this.colIndex
      }
    });
  });
  $('.datapt').mouseleave(function() {
    $('.info').text('');
    d3.select(row.leaves[this.rowIndex]).classed('active', false);
    d3.select(col.leaves[this.colIndex]).classed('active', false);
  });
  $('.colormap').mouseover(function() {
    el.classed('highlighting', true);
  });
  $('.colormap').mouseleave(function() {
    el.classed('highlighting', false);
  });
  
  return {
    on: function(type, listener) {
      dispatcher.on(type, listener);
      return this;
    }
  };
}
