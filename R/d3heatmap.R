#' @import htmlwidgets
#' @importFrom grDevices col2rgb rgb colorRampPalette
#' @importFrom stats as.dendrogram dendrapply dist hclust is.leaf order.dendrogram reorder sd
#' @importFrom utils capture.output str
NULL

#' D3 Heatmap widget
#' 
#' Creates a D3.js-based heatmap widget.
#' 
#' @param x A numeric matrix or data.frame with numeric columns. All non-numeric columns
#' will be filtered out
#' 
#' @param main \emph{string} Plot title. Defaults to \code{NULL}.
#' 
#' @param theme A custom CSS theme to use. Currently the only valid values are 
#'   \code{""} and \code{"dark"}. \code{"dark"} is primarily intended for 
#'   standalone visualizations, not R Markdown or Shiny.
#' 
#' @param col Either a colorbrewer2.org palette name (e.g. \code{"YlOrRd"} 
#' or \code{"Blues"}), or a vector of colors to interpolate in hexadecimal 
#' \code{"#RRGGBB"} format, or a color interpolation function like
#' \code{\link[grDevices]{colorRamp}}.
#' 
#' @param breaks \emph{integer} Either a numeric vector indicating the splitting 
#' points for binning x into colors, or a integer number of break points 
#' to be used, in which case the break points will be spaced equally 
#' between min(x) and max(x).
#' 
#' @param symbreaks \emph{logical} Arrange color bins symmetrically around zero?
#' 
#' @param Rowv determines if and how the row dendrogram 
#' should be reordered.	By default, it is TRUE, which implies 
#' dendrogram is computed and reordered based on row means. 
#' If NULL or FALSE, then no dendrogram is computed and no reordering 
#' is done. If a dendrogram, then it is used "as-is", i.e. without any 
#' reordering. If a vector of integers, then a dendrogram is computed 
#' and reordered based on the order of the vector.
#' 
#' @param Colv determines if and how the column dendrogram should be 
#' reordered.	Has the options as the Rowv argument above and additionally 
#' when x is a square matrix, Colv = "Rowv" means that columns should be 
#' treated identically to the rows.
#' 
#' @param distfun function used to compute the distance (dissimilarity) 
#' between both rows and columns. Defaults to dist.
#' 
#' @param hclustfun function used to compute the hierarchical clustering 
#' when Rowv or Colv are not dendrograms. Defaults to hclust.
#' 
#' @param dendrogram character string indicating whether to draw 'none', 
#' 'row', 'column' or 'both' dendrograms. Defaults to 'both'. However, if 
#' Rowv (or Colv) is FALSE or NULL and dendrogram is 'both', then a warning 
#' is issued and Rowv (or Colv) arguments are honored.
#' 
#' @param reorderfun function(d, w) of dendrogram and weights for reordering 
#' the row and column dendrograms. The default uses stats{reorder.dendrogram}
#' 
#' @param k_row,kr an integer scalar with the desired number of groups by which 
#' to color the dendrogram's branches in the rows 
#' (uses \link[dendextend]{color_branches})
#' 
#' @param k_col,kc an integer scalar with the desired number of groups by which 
#' to color the dendrogram's branches in the columns 
#' (uses \link[dendextend]{color_branches})
#' 
#' @param print.values \emph{logical} Show the values inside the cells. Defaults to \code{FALSE}.
#' 
#' @param key Show color key and density
#'    information? \code{TRUE/FALSE}. Defaults to \code{FALSE}
#' 
#' @param keysize \emph{numeric} value indicating the relative size of the key. Will multiple the labRowSize and labColSize.
#'  Defaults to 1
#'    
#' @param key.title Separate title for color key. Defaults to \code{NULL}.
#' 
#' @param key.location \emph{Required, character} Location for color key, either \code{"fl", "br", "tr"
#' "tl", or "bl"} for "float", "bottom right", "top right", "top left", and "bottom left". 
#' Defaults to \code{"fl"}, which follows the location of the axis labels.
#' 
#' @param density.info \emph{Optional, character} indicating whether to superimpose 
#' a 'histogram' or no plot ('none') on the color-key (legend). 
#' 
#' @param denscol \emph{Optional, character} giving the color for the density 
#' display specified by 'density.info', defaults to \code{NULL} which will fill the
#' bars with the same color as the bin
#' 
#' @param width Width in pixels (optional, defaults to automatic sizing).
#' 
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' 
#' @param labColSize Size of axes, in pixels.
#' 
#' @param labRowSize Size of axes, in pixels.
#' 
#' @param cexCol Font size of column labels, as a scalar to '12px' font. Default to 
#' 0.2 + 1/log10(ncol(x)).
#' 
#' @param cexRow Font size of row labels, as a scalar to '12px' font size. Defaults to 
#' 0.2 + 1/log10(nrow(x)).
#' 
#' @param srtCol Angle of axis labels. Defaults to 60. Maximum of 90 (vertical), minimum of 25.
#' 
#' @param sideCol,xaxis.location 3 or 1, for "bottom" or "top", which side column labels 
#' display. Defaults to 3 ("bottom"). For \code{xaxis.location} use "bottom"
#' or "top".
#' 
#' @param sideRow,yaxis.location 2 or 4, for "left" or "right", which side row labels 
#' display. Defaults to 4 ("right"). For \code{yaxis.location} use "left" or "right".
#' 
#' @param xlab Title text of x axis
#' 
#' @param ylab Title text of y axis
#' 
#' @param xaxis_title_font_size Font size of x axis title in pixels. Defaults to 14.
#' 
#' @param yaxis_title_font_size Font size of y axis title in pixels. Defaults to 14. 
#' 
#' @param brush_color The base color to be used for the brush. The brush will be
#'   filled with a low-opacity version of this color. \code{"#RRGGBB"} format 
#'   expected.
#' 
#' @param show_grid \code{TRUE} to show gridlines, \code{FALSE} to hide them, or
#'   a numeric value to specify the gridline thickness in pixels (can be a non-integer).
#' 
#' @param anim_duration Number of milliseconds to animate zooming in and out.
#'   For large \code{x} it may help performance to set this value to \code{0}.
#'  
#' @param symm logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
#' 
#' @param revC logical indicating if the column order should be reversed for plotting.
#' Default (when missing) - is FALSE, unless symm is TRUE.
#' This is useful for cor matrix.
#' 
#' @param scale character indicating if the values should be centered and scaled in either 
#' the row direction or the column direction, or none. The default is "none".
#'
#' @param scale.by.range logical indicating whether to scale rows or columns by
#' the range of each row and column. Setting this parameter to \code{TRUE} 
#' automatically sets \code{na.rm} to \code{TRUE}
#' 
#' @param na.rm logical indicating whether NA's should be removed.
#' 
#' @param na.value numeric indicating where NA's should be substituted to trigger the NA color.
#' 
#' @param na.color Color of NA values in heatmap. Defaults to neutral gray.
#' 
#' @param rng A vector of two numbers, namely the minimum and maximum value
#'   to use when determining the mapping from values to colors. This is useful
#'   when the range of values changes between heatmaps, but colors should be the
#'   same (optional, defaults to the minimum and maximum of \code{x}).
#'   
#' @param digits \emph{integer} indicating the number of decimal places to be used by \link{round} for 'label'.
#' 
#' @param notecol \emph{character} name or hex specifying the color of the values printed
#' inside the cells
#' 
#' @param notecex,cex.note \emph{numeric} scalar of 12 to determine pixel size of printed cell values.
#' inside the cells. If not specified, the minimum font size between the x and y axes.
#' 
#' @param cellnote_row \emph{character} Label to display next to the row value when the user hovers over the cell.
#'  If not specified, tries to match the \code{xlab}; if no axis title, defaults to "Row".
#' 
#' @param cellnote_col \emph{character} Label to display next to the column value when the user hovers over the cell. 
#' If not specified, tries to match the \code{ylab}; if no axis title, defaults to "Col".
#' 
#' @param cellnote_val \emph{character} Label to display next to the cell value when the user hovers over the cell.
#'  Defaults to "Value".
#' 
#' @param cellnote \emph{numeric} (optional) matrix of the same dimensions as \code{x} that has the human-readable version of each value, for displaying to the user on hover. If \code{NULL}, then \code{x} will be coerced using \code{\link{as.character}}.
#' If missing, it will use \code{x}, after rounding it based on the \code{digits} parameter.
#' 
#' @param cellnote_scale \emph{logical} (default is FALSE). IF cellnote is missing and x is used, should cellnote be scaled if x is also scaled?
#' 
#' @param labRow character vectors with row labels to use (from top to bottom); default to rownames(x).
#' 
#' @param labCol character vectors with column labels to use (from left to right); default to colnames(x).
#'         
#' @param ColSideColors (optional) character vector of length ncol(x), or matrix with
#' columns equal to ncol(x), containing the color names for a horizontal side bar that may 
#' be used to annotate the columns of x. \code{ColIndividualColors}, from heatmap.2, can
#' also be used
#'   
#' @param RowSideColors (optional) character vector of length nrow(x), or matrix with rows
#' equal to nrow(x), containing the color names for a vertical side bar that may be used to
#' annotate the rows of x. \code{RowIndividualColors}, from heatmap.3, can also be used
#'   
#' @param RowColorsPalette a palette of colors to use for RowSideColors if passing a non-color matrix
#'   
#' @param ColColorsPalette a palette of colors to use for ColSideColors if passing a non-color matrix
#'   
#' @param RowColorsNames the names of the variables for RowSideColors. Overrides colnames(RowSideColors)
#'   
#' @param ColColorsNames the names of the variables for ColSideColors. Overrides rownames(ColSideColors)
#' with rows equal to nrow(x), containing the color names for a vertical side bar that may be used to annotate the 
#' rows of x.
#' 
#' @param ... a catch for undocumented features or unused arguments from heatmap.2
#' or heatmap.3, to enable direct use of those formulations in d3heatmap
#'   
#' @import htmlwidgets
#'   
#' @source 
#' The interface was designed based on \link{heatmap} and \link[gplots]{heatmap.2}
#' 
#' @seealso 
#' \link{heatmap}, \link[gplots]{heatmap.2}
#' 
#' @examples 
#' \dontrun{
#' 
#' d3heatmap(mtcars, scale = "column", col = "Blues")
#' 
#' }
#' 
#' @export
d3heatmap <- function(x
	, main = NULL
  , width = NULL
  , height = NULL
  , show_grid = TRUE
  , anim_duration = 500
  , rng = NULL
  
	## dendrogram control
  , symm = FALSE
  , Rowv = TRUE
  , Colv = if (symm) "Rowv" else TRUE
  , distfun = dist
  , hclustfun = hclust
  , dendrogram = c("both", "row", "column", "none")
  , reorderfun = function(d, w) reorder(d, w)
  , k_row = NULL
  , k_col = NULL
  , kr
  , kc
  , revC = NULL
  
  ## data scaling
  , scale = c("none", "row", "column")
	, scale.by.range = FALSE
  , na.rm = TRUE
  , na.color = "#777777"
  , na.value = NA

  ## legend (color key)
  , key = FALSE
  , keysize = 1
  , key.title = NULL
  , key.location = c("fl", "br", "tr", "tl", "bl")
	, density.info = c('histogram', 'none')
	, denscol = NULL
	
  ## cellnote formatting
  , digits = 3L
  , cellnote = NULL
  , cellnote_scale = FALSE
  , cellnote_row = NULL
  , cellnote_col = NULL
  , cellnote_val = "Value"
  , brush_color = "#0000FF"
	, print.values = FALSE
	, notecol = '#222222'
	, notecex = 1
	, cex.note

	## color controls  
  , theme = NULL
  , col = "RdYlBu"
  , breaks = NULL
  , symbreaks = FALSE
	
	## axis controls
  , labRow = rownames(x)
  , labCol = colnames(x)
  , labColSize = 80
  , labRowSize = 120
  , cexCol = NULL
  , cexRow = NULL
  , srtCol = 60
  , sideCol = 3
  , sideRow = 4
	, xaxis.location
	, yaxis.location
  , xlab = NULL
  , ylab = NULL
  , xaxis_title_font_size = 14
  , yaxis_title_font_size = 14

	# side colors
  , ColSideColors = NULL
  , RowSideColors = NULL
  , RowColorsPalette = NULL
  , ColColorsPalette = NULL
  , RowColorsNames = NULL
  , ColColorsNames = NULL
	
	, ...

) {
  if(length(dim(x)) != 2) {
    stop(paste("x is not a 2-dimensional object. Here is the str output of x:", capture.output(str(x)), collapse = "\n"))
  }
  if(is.matrix(x) && !is.numeric(x)) {
    warning(paste(capture.output(str(x)), collapse = "\n"))
    stop("d3heatmap data must be a numeric 
      matrix or a data.frame with numeric columns")
  }
 
  # we'll modify this to take any matrix or data.frame, but filter out all
  # non-numeric columns 
  if (is.data.frame(x) | is.table(x))
    x <- x[, sapply(x, is.numeric)]
  
  opts <- list(...)
 
	axis.locations <- c("top", "left", "bottom", "right")
	
	## pre-process paraemters
  ##===========================================================
	## process the overlap of parameters between the old d3heatmap,
	## heatmap.2, and heatmap.3 APIs
   
  if (!missing(kr)) k_row <- kr
  if (!missing(kc)) k_col <- kc
  if (!missing(cex.note)) notecex <- cex.note
	
	if(!is.null(ColSideColors)) 
	  ColSideColors <- matrix(ColSideColors, ncol = ncol(x))
	
	if(!is.null(RowSideColors)) 
	  RowSideColors <- matrix(RowSideColors, nrow = nrow(x))
  
  if (!is.null(opts$ColIndividualColors)) 
    ColSideColors <- matrix(opts$ColIndividualColors, ncol = ncol(x))
	
  if (!is.null(opts$RowIndividualColors)) 
    RowSideColors <- matrix(opts$RowIndividualColors, nrow = nrow(x))

	if (is.null(RowColorsPalette)) 
					RowColorsPalette <- c('blue', 'orange', 'black')
	if (is.null(ColColorsPalette)) 
					ColColorsPalette <- c('cyan', 'maroon', 'grey')

	if (is.null(RowColorsNames)) RowColorsNames <- colnames(RowSideColors)
	if (is.null(ColColorsNames)) ColColorsNames <- rownames(ColSideColors)
	
	## additional controls for working with the gadget (shiny inputs)
  ##===========================================================
  if (!is.null(k_row)) if (is.na(k_row)) k_row <- NULL
  if (!is.null(k_col)) if (is.na(k_col)) k_col <- NULL
 
  if (!is.null(cexCol)) if (is.na(cexCol)) cexCol <- NULL
  if (!is.null(cexRow)) if (is.na(cexRow)) cexRow <- NULL
	
  if(!missing(xaxis.location)) {
    xaxis.location <- tolower(xaxis.location)
    if (xaxis.location %in% c('top', 'bottom'))
      sideCol <- which(axis.locations == xaxis.location)
  } 
	
	if(!missing(yaxis.location)) {
	  yaxis.location <- tolower(yaxis.location)
	  if (yaxis.location %in% c('left', 'right'))
      sideRow <- which(axis.locations == yaxis.location)
	} 
    
	## Save the parameters for later API calls
  ##===========================================================
	## For the updated API, we need to be able to access stuff passed into
	## the original call to d3heatmap before everything is processed (and there 
	## is a lot of processing going on here). Primarily we need those items
	## passed into the internal heatmap() function, which does the lion's share
	## of the generating
	params <- list(
		x = x
		, Rowv = Rowv
		, Colv = Colv
		, distfun = distfun
		, hclustfun = hclustfun
		, dendrogram = match.arg(dendrogram)
		, reorderfun = reorderfun
		, k_row = k_row
		, k_col = k_col
		, symm = symm
		, revC = revC
		, scale = match.arg(scale)
		, scale.by.range = scale.by.range
		, na.rm = na.rm
		, na.value = na.value
		, digits = digits
		, cellnote = cellnote
		, cellnote_scale = cellnote_scale
		, labRow = labRow
		, labCol = labCol
		, col = col
		, symbreaks = symbreaks
		, na.color = na.color
		, rng = rng
		, breaks = breaks
		, RowSideColors = RowSideColors
		, ColSideColors = ColSideColors
		, RowColorsPalette = RowColorsPalette
		, ColColorsPalette = ColColorsPalette
	)
	
  
	## the big call to create the heatmap
  ##==============================
	## inserting new call to the refactored heatmap function, which does everything up to 
	## this point that the old d3heatmap procedure did
	hm <- do.call(heatmap, args = params)
	x <- hm$x
	
	## Colors for the heatmap and the color key (legend)
  ##===========================================
	hm_colors <- heatmapColors(x
								, params$col
								, params$na.color
								, params$na.rm
								, params$rng
								, params$scale
								, params$breaks
								, params$symbreaks
							)

	## axis and cellnote labels sync
  ##==============================
  if (is.null(cellnote_row)) cellnote_row <- ylab
  if (is.null(cellnote_col)) cellnote_col <- xlab
	
	
	## font sizes
  ##==============================
  if (is.null(cexCol)) cexCol <- 0.2 + 1 / log10(ncol(x))
  if (is.null(cexRow)) cexRow <- 0.2 + 1 / log10(nrow(x))

	## bundling the options
  ##==============================================
	srtCol <- min(90, max(srtCol, 25))
	key.location <- match.arg(key.location)
	axis.locations <- c("top", "left", "bottom", "right")
  
	if (!sideCol %in% c(1, 3)) sideCol <- 3	
	if (!sideRow %in% c(2, 4)) sideRow <- 4	

  options <- list(
    hm_width = width
    , hm_height = height
    , xaxis_height = labColSize
    , yaxis_width = labRowSize
    , xaxis_font_size = cexCol * 12
    , yaxis_font_size = cexRow * 12
    , xaxis_angle = srtCol
    , xaxis_location = axis.locations[sideCol]
    , yaxis_location = axis.locations[sideRow]
    , xaxis_title = xlab
    , yaxis_title = ylab
    , xaxis_title_font_size = xaxis_title_font_size
    , yaxis_title_font_size = yaxis_title_font_size 
    , bins = hm_colors$breaks
		, manual_breaks = (length(breaks) > 1)
    , symbreaks = symbreaks
    , print_values = print.values
    , show_legend = key
    , legend_scaler = as.numeric(keysize[1])
    , legend_title = key.title
    , legend_location = key.location
		, legend_colors = hm_colors$legend_colors
		, legend_bins = hm_colors$legend_bins
		, legend_breaks = hm_colors$legend_breaks
	  , legend_type = match.arg(density.info)
	  , legend_fill = denscol
    , brush_color = brush_color
    , na_color = na.color
    , show_grid = show_grid
    , cellnote_row = cellnote_row
    , cellnote_col = cellnote_col
    , cellnote_val = cellnote_val
    , cellnote_fontsize = notecex * 12
    , cellnote_color = notecol
    , anim_duration = anim_duration
		, rsc_labs = hm$rsclabs
		, csc_labs = hm$csclabs
		, rsc_cols = hm$rsccols
		, csc_cols = hm$csccols
		, rsc_colnames = RowColorsNames
		, csc_colnames = ColColorsNames
  )

  if (is.null(hm$rowDend)) c(options, list(yclust_width = 0))
  if (is.null(hm$colDend)) c(options, list(xclust_height = 0))
  
  # transpose will cause error if row colors is null
  if (!is.null(hm$rowcolors)) hm$rowcolors <- t(hm$rowcolors)
 
 	## proceed to the widget
 	## NOTE: when loading the side colors, we transpose the
 	## rowSideColors
	##=======================================	
  imgUri <- encodeAsPNG(t(x), hm_colors$col)

  payload <- list(
    rows = hm$rowDend 
		, cols = hm$colDend
		, matrix = hm$mtx
		, title = main
		, image = imgUri
		, rowcolors = hm$rowcolors
		, colcolors = hm$colcolors
    , theme = theme 
		, options = options
		, params = params
	)
  
  # create widget
  htmlwidgets::createWidget(
    name = 'd3heatmap'
    , payload # the x param
    , width = width
    , height = height
    , package = 'd3heatmap'
    , sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
  )
}

#' @import png base64enc
encodeAsPNG <- function(x, colors) {
  colorData <- as.raw(col2rgb(colors(x), alpha = TRUE))
  dim(colorData) <- c(4, ncol(x), nrow(x))
  pngData <- png::writePNG(colorData)
  encoded <- base64enc::base64encode(pngData)
  paste0("data:image/png;base64,", encoded)
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
#' \dontrun{
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
#'       col = input$palette,
#'       dendrogram = if (input$cluster) "both" else "none"
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

