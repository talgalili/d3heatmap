#' D3 Heatmap widget
#'
#' Creates a D3.js-based heatmap widget.
#'
#' @import htmlwidgets
#'
#' @export
d3heatmap <- function(data, theme = "", width = NULL, height = NULL) {

  
  matrix <- as.matrix(data)
  
  rng <- range(matrix)
  
  #   rowClust <- hclust(dist(matrix))
  #   matrix <- matrix[rowClust$order,]
  #   colClust <- hclust(dist(t(matrix)))
  #   matrix <- matrix[,colClust$order]
  tmp <- tempfile()
  png(tmp)
  hm <- heatmap(data, keep.dendro=TRUE)
  dev.off()
  unlink(tmp)
  rowClust <- as.hclust(hm$Rowv)
  colClust <- as.hclust(hm$Colv)
  matrix <- matrix[hm$rowInd, hm$colInd]
  
  rowDend <- hclustToTree(rowClust)[[1]]
  colDend <- hclustToTree(colClust)[[1]]
  
  
  domain <- seq.int(rng[1], rng[2], length.out = 100)
  
  colors <- topo.colors(100)
  colors <- sub('FF$', '', colors)
  
  matrix <- list(data = as.numeric(t(matrix)),
    dim = dim(matrix),
    rows = row.names(matrix),
    cols = names(matrix),
    colors = colors,
    domain = domain)
  
  x <- list(rows = rowDend, cols = colDend, matrix = matrix, theme = theme)
  
  # create widget
  htmlwidgets::createWidget(
    name = 'd3heatmap',
    x,
    width = width,
    height = height,
    package = 'heatmap'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
d3heatmapOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'd3heatmap', width, height, package = 'heatmap')
}

#' Widget render function for use in Shiny
#'
#' @export
renderD3heatmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, d3heatmapOutput, env, quoted = TRUE)
}
