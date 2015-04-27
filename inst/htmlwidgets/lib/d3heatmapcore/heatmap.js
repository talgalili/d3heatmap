function heatmap(selector, data, options) {
  var el = d3.select(selector);

  var bbox = el.node().getBoundingClientRect();

  var Controller = function() {
    this._events = d3.dispatch("highlight", "datapoint_hover", "transform");
    this._highlight = {x: null, y: null};
    this._datapoint_hover = {x: null, y: null, value: null};
    this._transform = null;
  };
  (function() {
    this.highlight = function(x, y) {
      if (!arguments.length) return this._highlight;

      if (arguments.length == 1) {
        this._highlight = x;
      } else {
        this._highlight = {x: x, y: y};
      }
      this._events.highlight.call(this, this._highlight);
    };

    this.datapoint_hover = function(x, y, value) {
      if (!arguments.length) return this._datapoint_hover;
      
      if (arguments.length == 1) {
        this._datapoint_hover = x;
      } else {
        this._datapoint_hover = {x: x, y: y, value: value};
      }
      this._events.datapoint_hover.call(this, this._datapoint_hover);
    };

    this.transform = function(_) {
      if (!arguments.length) return this._transform;
      this._transform = _;
      this._events.transform.call(this, _);
    };

    this.on = function(evt, callback) {
      this._events.on(evt, callback);
    };
  }).call(Controller.prototype);

  var controller = new Controller();

  // Set option defaults
  var opts = {};
  options = options || {};
  opts.width = options.width || bbox.width;
  opts.height = options.height || bbox.height;
  opts.xclust_height = options.xclust_height || opts.height * 0.12;
  opts.yclust_width = options.yclust_width || opts.width * 0.12;
  opts.xaxis_height = options.xaxis_height || 120;
  opts.yaxis_width = options.yaxis_width || 120;
  opts.axis_padding = options.axis_padding || 3;

  if (!data.rows) {
    opts.yclust_width = 0;
  }
  if (!data.cols) {
    opts.xclust_height = 0;
  }

  var colormapBounds = {
    position: "absolute",
    left: opts.yclust_width,
    top: opts.xclust_height,
    width: opts.width - opts.yclust_width - opts.yaxis_width,
    height: opts.height - opts.xclust_height - opts.xaxis_height
  };
  var colDendBounds = {
    position: "absolute",
    left: colormapBounds.left,
    top: 0,
    width: colormapBounds.width,
    height: opts.xclust_height
  };
  var rowDendBounds = {
    position: "absolute",
    left: 0,
    top: colormapBounds.top,
    width: opts.yclust_width,
    height: colormapBounds.height
  };
  var yaxisBounds = {
    position: "absolute",
    top: colormapBounds.top,
    left: colormapBounds.left + colormapBounds.width,
    width: opts.yaxis_width,
    height: colormapBounds.height
  };
  var xaxisBounds = {
    position: "absolute",
    top: colormapBounds.top + colormapBounds.height,
    left: colormapBounds.left,
    width: colormapBounds.width,
    height: opts.xaxis_height
  };

  function cssify(styles) {
    return {
      position: styles.position,
      top: styles.top + "px",
      left: styles.left + "px",
      width: styles.width + "px",
      height: styles.height + "px"
    };
  }

  // Create DOM structure
  (function() {
    var inner = el.append("div").classed("inner", true);
    var info = inner.append("div").classed("info", true);
    var colDend = inner.append("svg").classed("dendrogram colDend", true).style(cssify(colDendBounds));
    var rowDend = inner.append("svg").classed("dendrogram rowDend", true).style(cssify(rowDendBounds));
    var colmap = inner.append("svg").classed("colormap", true).style(cssify(colormapBounds));
    var xaxis = inner.append("svg").classed("axis xaxis", true).style(cssify(xaxisBounds));
    var yaxis = inner.append("svg").classed("axis yaxis", true).style(cssify(yaxisBounds));
    
    // Hack the width of the x-axis to allow x-overflow of rotated labels; the
    // QtWebkit viewer won't allow svg elements to overflow:visible.
    xaxis.style("width", (opts.width - opts.yclust_width) + "px");
    xaxis
      .append("defs")
        .append("clipPath").attr("id", "xaxis-clip")
          .append("polygon")
            .attr("points", "" + [
              [0, 0],
              [xaxisBounds.width, 0],
              [xaxisBounds.width + yaxisBounds.width, xaxisBounds.height],
              [0, xaxisBounds.height]
            ]);
    xaxis.node(0).setAttribute("clip-path", "url(#xaxis-clip)");

    inner.on("click", function() {
      controller.highlight(null, null);
    });
    controller.on('highlight.inner', function(hl) {
      inner.classed('highlighting',
        typeof(hl.x) === 'number' || typeof(hl.y) === 'number');
    });
  })();
  
  var row = !data.rows ? null : dendrogram(el.select('svg.rowDend'), data.rows, false, rowDendBounds.width, rowDendBounds.height, opts.axis_padding);
  var col = !data.cols ? null : dendrogram(el.select('svg.colDend'), data.cols, true, colDendBounds.width, colDendBounds.height, opts.axis_padding);
  var colormap = colormap(el.select('svg.colormap'), data.matrix, colormapBounds.width, colormapBounds.height);
  var xax = axisLabels(el.select('svg.xaxis'), data.cols || data.matrix.cols, true, xaxisBounds.width, xaxisBounds.height, opts.axis_padding);
  var yax = axisLabels(el.select('svg.yaxis'), data.rows || data.matrix.rows, false, yaxisBounds.width, yaxisBounds.height, opts.axis_padding);
  
  function colormap(svg, data, width, height) {
    // Check for no data
    if (data.length === 0)
      return function() {};
    
    var cols = data.dim[1];
    var rows = data.dim[0];
    
    var merged = data.data;
    
    var x = d3.scale.linear().domain([0, cols]).range([0, width]);
    var y = d3.scale.linear().domain([0, rows]).range([0, height]);
    var color = d3.scale.linear()
        .domain(data.domain)
        .range(data.colors);
    
    var brush = d3.svg.brush()
        .x(x)
        .y(y)
        .clamp([true, true])
        .on('brush', function() {
          var extent = brush.extent();
          extent[0][0] = Math.round(extent[0][0]);
          extent[0][1] = Math.round(extent[0][1]);
          extent[1][0] = Math.round(extent[1][0]);
          extent[1][1] = Math.round(extent[1][1]);
          d3.select(this).call(brush.extent(extent));
        })
        .on('brushend', function() {

          if (brush.empty()) {
            controller.transform({
              scale: [1,1],
              translate: [0,0],
              extent: [[0,0],[cols,rows]]
            });
          } else {
            var ex = brush.extent();
            var scale = [
              cols / (ex[1][0] - ex[0][0]),
              rows / (ex[1][1] - ex[0][1])
            ];
            var translate = [-x(ex[0][0]) * scale[0], -y(ex[0][1]) * scale[1]];
            controller.transform({scale: scale, translate: translate, extent: ex});
          }
          brush.clear();
          d3.select(this).call(brush);
        });

    controller.on('transform.colormap', function(_) {
      svg.transition().ease('linear').duration(500)
          .attr('transform', 'translate(' + _.translate + ') scale(' + _.scale + ')');
    });
    
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
        .property("value", function(d, i) { return d; })
        .attr("x", function(d, i) {
          return x(i % cols);
        })
        .attr("y", function(d, i) {
          return y(Math.floor(i / cols));
        })
        .attr("width", x(1))
        .attr("height", y(1))
        .attr("fill", function(d) {
          if (d === null) {
            return "transparent";
          }
          return color(d);
        });
    rect.append("title")
        .text(function(d, i) { return (d === null) ? "NA" : d + ""; });

    svg.append("g")
        .attr('class', 'brush')
        .call(brush)
        .call(brush.event);

    controller.on('highlight.datapt', function(hl) {
      rect.classed('highlight', function(d, i) {
        return (this.rowIndex === hl.y) || (this.colIndex === hl.x);
      });
    });
  }

  function axisLabels(svg, data, rotated, width, height, padding) {
    svg = svg.append('g');

    // The data variable is either cluster info, or a flat list of names.
    // If the former, transform it to simply a list of names.
    var leaves;
    if (data.children) {
      leaves = d3.layout.cluster().nodes(data)
          .filter(function(x) { return !x.children; })
          .map(function(x) { return x.name + ""; });
    } else if (data.length) {
      leaves = data;
    }
    
    // Define scale, axis
    var scale = d3.scale.ordinal()
        .domain(leaves)
        .rangeBands([0, rotated ? width : height]);
    var axis = d3.svg.axis()
        .scale(scale)
        .orient(rotated ? "bottom" : "right")
        .outerTickSize(0)
        .tickPadding(padding)
        .tickValues(leaves);

    // Create the actual axis
    var axisNodes = svg.append("g")
        .attr("transform", rotated ? "translate(0," + padding + ")" : "translate(" + padding + ",0)")
        .call(axis);
    var fontSize = Math.min(18, Math.max(9, scale.rangeBand() - (rotated ? 11: 8)));
    axisNodes.selectAll("text").style("font-size", fontSize + "px");
    
    var mouseTargets = svg.append("g")
      .selectAll("g").data(leaves);
    mouseTargets
      .enter()
        .append("g").append("rect")
          .attr("transform", rotated ? "rotate(45),translate(0,0)" : "")
          .attr("fill", "transparent")
          .on("click", function(d, i) {
            var hl = {x: null, y: null};
            if (rotated)
              hl.x = i;
            else
              hl.y = i;
            controller.highlight(hl);
            d3.event.stopPropagation();
          });
    function layoutMouseTargets(selection) {
      selection
          .attr("transform", function(d, i) {
            var x = rotated ? scale(d) + scale.rangeBand()/2 : 0;
            var y = rotated ? padding + 6 : scale(d);
            return "translate(" + x + "," + y + ")";
          })
        .selectAll("rect")
          .attr("height", scale.rangeBand() / (rotated ? 1.414 : 1))
          .attr("width", rotated ? height * 1.414 * 1.2 : width);
    }
    layoutMouseTargets(mouseTargets);

    if (rotated) {
      axisNodes.selectAll("text")
        .attr("transform", "rotate(45),translate(6, 0)")
        .style("text-anchor", "start");
    }
    
    controller.on('highlight.axis-' + (rotated ? 'x' : 'y'), function(hl) {
      var ticks = axisNodes.selectAll('.tick');
      var selected = hl[rotated ? 'x' : 'y'];
      if (typeof(selected) !== 'number') {
        ticks.classed('faded', false);
        return;
      }
      ticks.classed('faded', function(d, i) {
        return i !== selected;
      });
    });

    controller.on('transform.axis-' + (rotated ? 'x' : 'y'), function(_) {
      var dim = rotated ? 0 : 1;
      //scale.domain(leaves.slice(_.extent[0][dim], _.extent[1][dim]));
      var rb = [_.translate[dim], (rotated ? width : height) * _.scale[dim] + _.translate[dim]];
      scale.rangeBands(rb);
      var tAxisNodes = axisNodes.transition().duration(500).ease('linear');
      tAxisNodes.call(axis);
      tAxisNodes.selectAll("g")
          .style("opacity", function(d, i) {
            if (i >= _.extent[0][dim] && i < _.extent[1][dim]) {
              return 1;
            } else {
              return 0;
            }
          });
      tAxisNodes
        .selectAll("text")
          .style("text-anchor", "start");
      mouseTargets.transition().duration(500).ease('linear')
          .call(layoutMouseTargets)
          .style("opacity", function(d, i) {
            if (i >= _.extent[0][dim] && i < _.extent[1][dim]) {
              return 1;
            } else {
              return 0;
            }
          });
    });

  }
  
  function dendrogram(svg, data, rotated, width, height, padding) {
    var x = d3.scale.linear();
    var y = d3.scale.linear()
        .domain([0, height])
        .range([0, height]);
    
    var cluster = d3.layout.cluster()
        .separation(function(a, b) { return 1; })
        .size([rotated ? width : height, (rotated ? height : width) - padding - 1]);
    
    var transform = "translate(1,0)";
    if (rotated) {
      // Flip dendrogram vertically
      x.range([1, 0]);
      // Rotate
      transform = "rotate(-90) translate(-2,0)";
    }

    var dendrG = svg
        .attr("width", width)
        .attr("height", height)
      .append("g")
        .attr("transform", transform);
    
    var nodes = cluster.nodes(data),
        links = cluster.links(nodes);

    // I'm not sure why, but after the heatmap loads the "links"
    // array mutates to much smaller values. I can't figure out
    // what's doing it, so instead we just make a deep copy of
    // the parts we want.
    var links1 = links.map(function(link, i) {
      return {
        source: {x: link.source.x, y: link.source.y},
        target: {x: link.target.x, y: link.target.y}
      };
    });
    
    var lines = dendrG.selectAll("polyline").data(links1);
    lines
      .enter().append("polyline")
        .attr("class", "link");

    function draw(selection) {
      function elbow(d, i) {
        return x(d.source.y) + "," + y(d.source.x) + " " +
            x(d.source.y) + "," + y(d.target.x) + " " +
            x(d.target.y) + "," + y(d.target.x);
      }
      
      selection
          .attr("points", elbow);
    }

    controller.on('transform.dendr-' + (rotated ? 'x' : 'y'), function(_) {
      var scaleBy = _.scale[rotated ? 0 : 1];
      var translateBy = _.translate[rotated ? 0 : 1];
      y.range([translateBy, height * scaleBy + translateBy]);
      draw(lines.transition().duration(500).ease("linear"));
    });

    draw(lines);
  }

 
  var dispatcher = d3.dispatch('hover', 'click');
  
  function on_col_label_mouseenter(e) {
    controller.highlight(+d3.select(this).attr("index"), null);
  }
  function on_col_label_mouseleave(e) {
    controller.highlight(null, null);
  }
  function on_row_label_mouseenter(e) {
    controller.highlight(null, +d3.select(this).attr("index"));
  }
  function on_row_label_mouseleave(e) {
    controller.highlight(null, null);
  }

  return {
    on: function(type, listener) {
      dispatcher.on(type, listener);
      return this;
    }
  };
}
