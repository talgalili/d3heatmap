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
#' The interface was designed based on \link{heatmap} and \link[gplots]{heatmap.2}
#' 
#' @seealso 
#' \link{heatmap}, \link[gplots]{heatmap.2}
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

  if(!is.matrix(x)) {
    # warning("x was coerced to a matrix using as.matrix")
    x <- as.matrix(x)
  }

  ############### gplots::heatmap.2 code (some is commented out) ##############
  
  retval <- list()
  
  scale <- if(symm && missing(scale)) "none" else match.arg(scale)
  dendrogram <- match.arg(dendrogram)
  
  
  
  # TODO: This is redundant. we'd probably want to remove cluster (or dendrogram?!) from the list of parameters
  if(!cluster) {
    Rowv <- FALSE
    Colv <- FALSE
    dendrogram <- "none"
  } else {
    if(dendrogram == "row") Colv <- FALSE
    if(dendrogram == "column") Rowv <- FALSE
    if(dendrogram == "none") Rowv <- Colv <- FALSE
  }
  
  
  
  
#   trace <- match.arg(trace)
#   density.info <- match.arg(density.info)
#   
#   if(length(col)==1 && is.character(col) )
#     col <- get(col, mode="function")
#   
#   if(!missing(breaks) && (scale!="none"))
#     warning("Using scale=\"row\" or scale=\"column\" when breaks are",
#             "specified can produce unpredictable results.",
#             "Please consider using only one or the other.")
  
  if ( is.null(Rowv) || is.na(Rowv) )
    Rowv <- FALSE
  if ( is.null(Colv) || is.na(Colv) )
    Colv <- FALSE
  else if( all(Colv=="Rowv") )
    Colv <- Rowv
  
  if(length(di <- dim(x)) != 2 || !is.numeric(x))
    stop("`x' must be a numeric matrix")

  nr <- di[1]
  nc <- di[2]
  
  ### TODO: debating if to include this or not:
#   
#   if(nr <= 1 || nc <= 1)
#     stop("`x' must have at least 2 rows and 2 columns")
#   
#   if(!is.numeric(margins) || length(margins) != 2)
#     stop("`margins' must be a numeric vector of length 2")
#   
#   if(missing(cellnote))
#     cellnote <- matrix("", ncol=ncol(x), nrow=nrow(x))
  
  is.dendrogram <- function (x) { inherits(x, "dendrogram")  }
  
  
  if(!is.dendrogram(Rowv)) {
    ## Check if Rowv and dendrogram arguments are consistent
    if (
      (
        ( is.logical(Rowv)  && !isTRUE(Rowv) )
        ||
        ( is.null(Rowv) )
      )
      &&
      ( dendrogram %in% c("both","row") )
    )
    {
      if (is.logical(Colv) && (Colv))
        dendrogram <- "column"
      else
        dendrogram <- "none"
      
      warning("Discrepancy: Rowv is FALSE, while dendrogram is `",
              dendrogram, "'. Omitting row dendogram.")
      
    }
  }
  
  if(!is.dendrogram(Colv)) {
    ## Check if Colv and dendrogram arguments are consistent
    if (
      (
        (is.logical(Colv) && !isTRUE(Colv) )
        ||
        (is.null(Colv))
      )
      &&
      ( dendrogram %in% c("both","column")) )
    {
      if (is.logical(Rowv) && (Rowv))
        dendrogram <- "row"
      else
        dendrogram <- "none"
      
      warning("Discrepancy: Colv is FALSE, while dendrogram is `",
              dendrogram, "'. Omitting column dendogram.")
    }
  }
  
  
  ## by default order by row/col mean
  ## if(is.null(Rowv)) Rowv <- rowMeans(x, na.rm = na.rm)
  ## if(is.null(Colv)) Colv <- colMeans(x, na.rm = na.rm)
  
  ## get the dendrograms and reordering indices
  
  ## if( dendrogram %in% c("both","row") )
  ##  { ## dendrogram option is used *only* for display purposes
  if(is.dendrogram(Rowv))
  {
    ddr <- Rowv ## use Rowv 'as-is', when it is dendrogram
    rowInd <- order.dendrogram(ddr)
    if(length(rowInd)>nr || any(rowInd<1 | rowInd > nr ))
      stop("Rowv dendrogram doesn't match size of x")
    if (length(rowInd) < nr)
      nr <- length(rowInd)
  }
  else if (is.integer(Rowv))
  {
    ## Compute dendrogram and do reordering based on given vector
    distr <- distfun(x)
    hcr <- hclustfun(distr)
    ddr <- as.dendrogram(hcr)
    ddr <- reorderfun(ddr, Rowv)
    
    rowInd <- order.dendrogram(ddr)
    if(nr != length(rowInd))
      stop("row dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Rowv))
  { ## If TRUE, compute dendrogram and do reordering based on rowMeans
    Rowv <- rowMeans(x, na.rm = na.rm)
    distr <- distfun(x)
    hcr <- hclustfun(distr)
    ddr <- as.dendrogram(hcr)
    ddr <- reorderfun(ddr, Rowv)
    
    rowInd <- order.dendrogram(ddr)
    if(nr != length(rowInd))
      stop("row dendrogram ordering gave index of wrong length")
  } else {
    rowInd <- nr:1
  }
  
  ## if( dendrogram %in% c("both","column") )
  ##   {
  if(is.dendrogram(Colv))
  {
    ddc <- Colv ## use Colv 'as-is', when it is dendrogram
    colInd <- order.dendrogram(ddc)
    if(length(colInd)>nc || any(colInd<1 | colInd > nc ))
      stop("Colv dendrogram doesn't match size of x")
    if (length(colInd) < nc)
      nc <- length(colInd)
  }
  else if(identical(Colv, "Rowv")) {
    if(nr != nc)
      stop('Colv = "Rowv" but nrow(x) != ncol(x)')
    if(exists("ddr"))
    {
      ddc <- ddr
      colInd <- order.dendrogram(ddc)
    } else
      colInd <- rowInd
  } else if(is.integer(Colv))
  {## Compute dendrogram and do reordering based on given vector
    distc <- distfun(if(symm)x else t(x))
    hcc <- hclustfun(distc)
    ddc <- as.dendrogram(hcc)
    ddc <- reorderfun(ddc, Colv)
    
    colInd <- order.dendrogram(ddc)
    if(nc != length(colInd))
      stop("column dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Colv))
  {## If TRUE, compute dendrogram and do reordering based on rowMeans
    Colv <- colMeans(x, na.rm = na.rm)
    distc <- distfun(if(symm)x else t(x))
    hcc <- hclustfun(distc)
    ddc <- as.dendrogram(hcc)
    ddc <- reorderfun(ddc, Colv)
    
    colInd <- order.dendrogram(ddc)
    if(nc != length(colInd))
      stop("column dendrogram ordering gave index of wrong length")
  }
  else
  {
    colInd <- 1:nc
  }
  
#   retval$rowInd <- rowInd
#   retval$colInd <- colInd
#   retval$call <- match.call()
  
  
  ## reorder x & cellnote
  x <- x[rowInd, colInd]
  x.unscaled <- x
  # cellnote <- cellnote[rowInd, colInd]
  
#   if(is.null(labRow))
#     labRow <- if(is.null(rownames(x))) (1:nr)[rowInd] else rownames(x)
#   else
#     labRow <- labRow[rowInd]
#   
#   if(is.null(labCol))
#     labCol <- if(is.null(colnames(x))) (1:nc)[colInd] else colnames(x)
#   else
#     labCol <- labCol[colInd]
#   
#   if(!is.null(colRow))
#     colRow <- colRow[rowInd]
#   
#   if(!is.null(colCol))
#     colCol <- colCol[colInd]
  
  if(scale == "row") {
    retval$rowMeans <- rm <- rowMeans(x, na.rm = na.rm)
    x <- sweep(x, 1, rm)
    retval$rowSDs <-  sx <- apply(x, 1, sd, na.rm = na.rm)
    x <- sweep(x, 1, sx, "/")
  }
  else if(scale == "column") {
    retval$colMeans <- rm <- colMeans(x, na.rm = na.rm)
    x <- sweep(x, 2, rm)
    retval$colSDs <-  sx <- apply(x, 2, sd, na.rm = na.rm)
    x <- sweep(x, 2, sx, "/")
  }
  
  ############### binding code between heatmap.2 and d3heatmap code ##############
  
  rowDend <- if(exists("ddr") && is.dendrogram(ddr)) dendToTree(ddr) else NULL  
  colDend <- if(exists("ddc") && is.dendrogram(ddc)) dendToTree(ddc) else NULL  

  ############### original d3heatmap code ##############
  
  matrix <- as.matrix(x)
  rng <- range(matrix, na.rm = TRUE)
  
  #   rowClust <- hclust(dist(matrix))
  #   matrix <- matrix[rowClust$order,]
  #   colClust <- hclust(dist(t(matrix)))
  #   matrix <- matrix[,colClust$order]
  ### TODO: this was commented out due to the new code (will be removed later)
#   rowDend <- NULL
#   colDend <- NULL
  options <- NULL

  if (cluster) {
    tmp <- tempfile()
    png(tmp)
    heatmap_options$x = quote(matrix)
    heatmap_options$keep.dendro = TRUE
    hm <- do.call(stats::heatmap, heatmap_options)
    dev.off()
    unlink(tmp)

    ### TODO: this was commented out due to the new code (will be removed later)
#     if (length(hm$Rowv) > 0) {
#       # Reverse Rowv because this is how stats::heatmap draws it.
#       # It would also make sense to not reverse the data here but in
#       # the d3 code just draw from bottom to top, but that would be
#       # a *lot* more work at this point.
#       rowDend <- dendToTree(rev(hm$Rowv))
#     }
#     if (length(hm$Colv) > 0) {
#       colDend <- dendToTree(hm$Colv)
#     }

    # VERY IMPORTANT that rowInd be reversed, because we're calling
    # rev(hm$Rowv) above.
    
    # TODO: this is commented out since it introduced a bug that made incorrect connections between row/col names and values!
    # matrix <- matrix[rev(hm$rowInd), hm$colInd]
    
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














if(FALSE) {

  # basic example of using dendextend:
  
  x <- mtcars[c(2:4,7),1:4]
  library(dendextend)
  row_dend2 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 3)
  col_dend2 <- x %>% t %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 2)
  library(d3heatmap)
  d3heatmap(x, Rowv = row_dend2, Colv = col_dend2) # Works!
  
  row_dend2 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 3) %>% 
    set("branches_lwd", c(4,1)) %>%    
    set("branches_lty", c(1,1,3))
  plot(row_dend2)
  # for now, d3heatmap still ignores line type and width:
  d3heatmap(x, Rowv = row_dend2, Colv = col_dend2) # Works!
  
  
  
  # various examples  
  library(d3heatmap)
  d3heatmap(scale(mtcars), colors = "Greens", theme = "dark")
  d3heatmap(scale(mtcars), cluster = FALSE)
  
  d3heatmap(scale(mtcars))
  d3heatmap(scale(mtcars), cluster = FALSE)
  d3heatmap(scale(mtcars), Colv = FALSE)
  d3heatmap(scale(mtcars), Rowv = FALSE)
  d3heatmap(scale(mtcars), dendrogram = "row")
  d3heatmap(scale(mtcars), dendrogram = "col")
  d3heatmap(scale(mtcars), dendrogram = "none") # like cluster = FALSE
  
  x <- mtcars[c(2:4,7),1:4]
  d3heatmap(x)
  d3heatmap(x, cluster = FALSE)
  
  d3heatmap(x, scale = "row")
  scale(x)
  d3heatmap(scale(x), cluster = FALSE)
  d3heatmap(x, scale = "column", cluster = FALSE)
  d3heatmap(x, scale = "row", cluster = FALSE)
  
  heatmap(scale(mtcars[1:4,1:4]), Rowv = NA, Colv = NA)
  
  
  library(dendextend)
  d3heatmap(x)
  # gives the same results - yay:
  row_dend <- x %>% dist %>% hclust %>% as.dendrogram # %>% plot
  d3heatmap(x, Rowv = row_dend)
  row_dend2 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 2)
  plot(row_dend2)
  d3heatmap:::dendToTree(row_dend2[[2]][[2]])
  d3heatmap(x, Rowv = row_dend2) # Works!
  
  # Next step!
  row_dend3 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    set("branches_lwd", c(4,1)) %>%    
    set("branches_lty", c(1,1,3)) %>%  
    set("branches_col", c("gold","grey","blue", "red")) 
  # set("branches_col", c(1,2,3)) # TODO: this doesn't work - needs to be fixed...
  plot(row_dend3) # line width and line type are still ignored.
  d3heatmap(x, Rowv = row_dend3) 
  
}