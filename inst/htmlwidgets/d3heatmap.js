HTMLWidgets.widget({

  name: 'd3heatmap',

  type: 'output',

  initialize: function(el, width, height) {

    return {
    };

  },

  renderValue: function(el, x, instance) {

    el.innerHTML = "";
    var hm = heatmap(el, x);
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

  }

});
