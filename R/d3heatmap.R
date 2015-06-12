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
#' @param invert_colors If \code{TRUE}, maps the colors specified in \code{colors}
#'   in descending rather than ascending order.
#' @param width Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' 
#' @param xaxis_height Size of axes, in pixels.
#' @param yaxis_width Size of axes, in pixels.
#' @param xaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
#' @param yaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
#' 
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
#'   
#' @param Rowv determines if and how the row dendrogram should be reordered.	By default, it is TRUE, which implies dendrogram is computed and reordered based on row means. If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a dendrogram, then it is used "as-is", ie without any reordering. If a vector of integers, then dendrogram is computed and reordered based on the order of the vector.
#' @param Colv determines if and how the column dendrogram should be reordered.	Has the options as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means that columns should be treated identically to the rows.
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns. Defaults to dist.
#' @param hclustfun function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms. Defaults to hclust.
#' @param dendrogram character string indicating whether to draw 'none', 'row', 'column' or 'both' dendrograms. Defaults to 'both'. However, if Rowv (or Colv) is FALSE or NULL and dendrogram is 'both', then a warning is issued and Rowv (or Colv) arguments are honoured.
#' @param reorderfun function(d, w) of dendrogram and weights for reordering the row and column dendrograms. The default uses stats{reorder.dendrogram}
#' 
#' @param symm logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
#' @param scale character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none".
#' @param na.rm logical indicating whether NA's should be removed.
#'   
#' @import htmlwidgets
#'   
#' @export
#' @source 
#' The interface was designed based on \link{heatmap} and \link[gplots2]{heatmap.2}
#' 
#' @seealso 
#' \link{heatmap}, \link[gplots2]{heatmap.2}
#' 
#' @examples 
#' \dontrun{
#' 
#' library(d3heatmap)
#' d3heatmap(scale(mtcars), colors = "Greens", theme = "dark")
#' d3heatmap(scale(mtcars), cluster = FALSE)
#' 
#' }
#' 
d3heatmap <- function(x,

                      
# TODO: we must add some control on the values we see in the tooltip, the level of precision is ridiculous                                            
    ## Also - it would be nice to have the row/col labels when hovering a cell...
                      
    ## dendrogram control
    Rowv = TRUE,
    Colv=if(symm)"Rowv" else TRUE,
    distfun = dist,
    hclustfun = hclust,
    dendrogram = c("both","row","column","none"),
    reorderfun = function(d, w) reorder(d, w),
    symm = FALSE,
                      
    ## data scaling
    scale = c("none","row", "column"),
    na.rm=TRUE,
    
    ##TODO: decide later which names/conventions to keep
  cluster = !any(is.na(x)),
  theme = NULL,
  colors = "RdYlBu",
  invert_colors = FALSE,
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
      # Reverse Rowv because this is how stats::heatmap draws it.
      # It would also make sense to not reverse the data here but in
      # the d3 code just draw from bottom to top, but that would be
      # a *lot* more work at this point.
      rowDend <- dendToTree(rev(hm$Rowv))
    }
    if (length(hm$Colv) > 0) {
      colDend <- dendToTree(hm$Colv)
    }

    # VERY IMPORTANT that rowInd be reversed, because we're calling
    # rev(hm$Rowv) above.
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
  
  colors <- scales::col_numeric(colors, 1:100)(
    if (invert_colors) 100:1 else 1:100
  )

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