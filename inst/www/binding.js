var heatmapOutputBinding = new Shiny.OutputBinding();
$.extend(heatmapOutputBinding, {
  find: function(scope) {
    return scope.find('.d3-heatmap');
  },
  renderValue: function(el, data) {
    var id = this.getId(el);
    var hm = heatmap(el, data);
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
});
Shiny.outputBindings.register(heatmapOutputBinding, 'com.rstudio.heatmap');
