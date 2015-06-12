#' @import htmlwidgets
NULL

`%||%` <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

#' D3 Heatmap widget
#' 
#' Creates a D3.js-based heatmap widget.
#' 
#' @param x A numeric matrix
#' @param cluster \code{TRUE} to perform clustering and show dendrograms. 
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#' @param theme A custom CSS theme to use. Currently the only valid values are 
#'   \code{""} and \code{"dark"}. \code{"dark"} is primarily intended for 
#'   standalone visualizations, not R Markdown or Shiny.
#' @param colors Either a colorbrewer2.org palette name (e.g. \code{"YlOrRd"} or
#'   \code{"Blues"}), or a vector of colors to interpolate in hexadecimal 
#'   \code{"#RRGGBB"} format.
#' @param width Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' @param xaxis_height,yaxis_width Size of axes, in pixels.
#' @param xaxis_font_size,yaxis_font_size Font size of axis labels, as a CSS 
#'   size (e.g. \code{"14px"} or \code{"12pt"}).
#' @param brush_color The base color to be used for the brush. The brush will be
#'   filled with a low-opacity version of this color. \code{"#RRGGBB"} format 
#'   expected.
#' @param show_grid \code{TRUE} to show gridlines, \code{FALSE} to hide them, or
#'   a numeric value to specify the gridline thickness in pixels (can be a 
#'   non-integer).
#' @param anim_duration Number of milliseconds to animate zooming in and out.
#'   For large \code{x} it may help performance to set this value to \code{0}.
#' @param heatmap_options List of options to pass to
#'   \code{\link[stats]{heatmap}}. Mostly only the clustering/dendrogram
#'   related arguments are interesting.
#'   
#' @import htmlwidgets
#'   
#' @export
d3heatmap <- function(x,
  cluster = !any(is.na(x)),
  theme = NULL,
  colors = "RdYlBu",
  width = NULL, height = NULL,
  xaxis_height = 120,
  yaxis_width = 120,
  xaxis_font_size = NULL,
  yaxis_font_size = NULL,
  brush_color = "#0000FF",
  show_grid = TRUE,
  anim_duration = 500,
  heatmap_options = list()
  ) {
  matrix <- as.matrix(x)
  rng <- range(matrix, na.rm = TRUE)
  
  #   rowClust <- hclust(dist(matrix))
  #   matrix <- matrix[rowClust$order,]
  #   colClust <- hclust(dist(t(matrix)))
  #   matrix <- matrix[,colClust$order]
  rowDend <- NULL
  colDend <- NULL
  options <- NULL

  if (cluster) {
    tmp <- tempfile()
    png(tmp)
    heatmap_options$x = quote(matrix)
    heatmap_options$keep.dendro = TRUE
    hm <- do.call(stats::heatmap, heatmap_options)
    dev.off()
    unlink(tmp)
    
    if (length(hm$Rowv) > 0) {
      # reverse = TRUE because this is how stats::heatmap draws it.
      # It would also make sense to not reverse the data here but in
      # the d3 code just draw from bottom to top, but that would be
      # a *lot* more work at this point.
      rowDend <- dendToTree(hm$Rowv, reverse = TRUE)
    }
    if (length(hm$Colv) > 0) {
      colDend <- dendToTree(hm$Colv)
    }

    # VERY IMPORTANT that rowInd be reversed, because we're calling
    # dendToTree(hm$Rowv, reverse = TRUE).
    matrix <- matrix[rev(hm$rowInd), hm$colInd]
    
  } else {
    # No clustering
    options <- c(options, list(xclust_height = 0, yclust_width = 0))
  }
  
  options <- c(options, list(
    xaxis_height = xaxis_height,
    yaxis_width = yaxis_width,
    xaxis_font_size = xaxis_font_size,
    yaxis_font_size = yaxis_font_size,
    brush_color = brush_color,
    show_grid = show_grid,
    anim_duration = anim_duration
  ))
  
  domain <- seq.int(rng[1], rng[2], length.out = 100)
  
  colors <- scales::col_numeric(colors, 1:100)(1:100)

  matrix <- list(data = as.numeric(t(matrix)),
    dim = dim(matrix),
    rows = row.names(matrix) %||% paste(1:nrow(matrix)),
    cols = colnames(matrix) %||% paste(1:ncol(matrix)),
    colors = colors,
    domain = domain)
  
  x <- list(rows = rowDend, cols = colDend, matrix = matrix, theme = theme, options = options)
  
  # create widget
  htmlwidgets::createWidget(
    name = 'd3heatmap',
    x,
    width = width,
    height = height,
    package = 'd3heatmap',
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
  )
}

#' Wrapper functions for using d3heatmap in shiny
#' 
#' Use \code{d3heatmapOutput} to create a UI element, and \code{renderD3heatmap}
#' to render the heatmap.
#' 
#' @param outputId Output variable to read from
#' @param width,height The width and height of the map (see
#'   \link[htmlwidgets]{shinyWidgetOutput})
#' @param expr An expression that generates a \code{\link{d3heatmap}} object
#' @param env The environment in which to evaluate \code{expr}
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @examples 
#' \donttest{
#' library(d3heatmap)
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   h1("A heatmap demo"),
#'   selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
#'   checkboxInput("cluster", "Apply clustering"),
#'   d3heatmapOutput("heatmap")
#' )
#' 
#' server <- function(input, output, session) {
#'   output$heatmap <- renderD3heatmap({
#'     d3heatmap(
#'       scale(mtcars),
#'       colors = input$palette,
#'       cluster = input$cluster
#'     )
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
#'   
#' @export
d3heatmapOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'd3heatmap', width, height, package = 'd3heatmap')
}

#' @rdname d3heatmapOutput
#' @export
renderD3heatmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, d3heatmapOutput, env, quoted = TRUE)
}
