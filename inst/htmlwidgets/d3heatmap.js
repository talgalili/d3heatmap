HTMLWidgets.widget({

  name: 'd3heatmap',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      lastTheme: null,
      lastValue: null
    };

  },

  renderValue: function(el, x, instance) {
    
    instance.lastValue = x;
    
    if (instance.lastTheme && instance.lastTheme != x.theme) {
      d3.select(document.body).classed("theme-" + instance.lastTheme, false);
    }
    if (x.theme) {
      d3.select(document.body).classed("theme-" + x.theme, true);
    }

    el.innerHTML = "";
    var hm = heatmap(el, x, x.options);
    if (window.Shiny) {
      var id = this.getId(el);
      hm.on('hover', function(e) {
        Shiny.onInputChange(id + '_hover', !e.data ? e.data : {
          value: e.data.value,
          row: e.data.row + 1,
          col: e.data.col + 1
        });
      });
      hm.on('click', function(e) {
        Shiny.onInputChange(id + '_click', !e.data ? e.data : {
          value: e.data.value,
          row: e.data.row + 1,
          col: e.data.col + 1
        });
      });
	  }
  },

  resize: function(el, width, height, instance) {
    if (instance.lastValue) {
      this.renderValue(el, instance.lastValue, instance);
    }
  }

});
