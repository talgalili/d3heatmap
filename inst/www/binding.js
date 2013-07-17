var heatmapOutputBinding = new Shiny.OutputBinding();
$.extend(heatmapOutputBinding, {
  find: function(scope) {
    return scope.find('.d3-heatmap');
  },
  renderValue: function(el, data) {
    heatmap(el, data);
  }
});
Shiny.outputBindings.register(heatmapOutputBinding, 'com.rstudio.heatmap');
