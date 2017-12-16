#' @import htmlwidgets
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats as.dendrogram dendrapply dist hclust is.leaf order.dendrogram reorder sd
NULL

#' D3 Heatmap widget
#' 
#' Creates a D3.js-based heatmap widget.
#' 
#' @param x A numeric matrix
#'   Defaults to \code{TRUE} unless \code{x} contains any \code{NA}s.
#' @param main \emph{string} Plot title. Defaults to \code{NULL}.
#' @param theme A custom CSS theme to use. Currently the only valid values are 
#'   \code{""} and \code{"dark"}. \code{"dark"} is primarily intended for 
#'   standalone visualizations, not R Markdown or Shiny.
#' @param colors Either a colorbrewer2.org palette name (e.g. \code{"YlOrRd"} or
#'   \code{"Blues"}), or a vector of colors to interpolate in hexadecimal 
#'   \code{"#RRGGBB"} format, or a color interpolation function like
#'   \code{\link[grDevices]{colorRamp}}.
#' @param bins \emph{integer} The number of colors to generate from the palette.
#' @param symbreaks \emph{logical} Arrange color bins symmetrically around zero?
#' @param Rowv determines if and how the row dendrogram 
#' should be reordered.	By default, it is TRUE, which implies 
#' dendrogram is computed and reordered based on row means. 
#' If NULL or FALSE, then no dendrogram is computed and no reordering 
#' is done. If a dendrogram, then it is used "as-is", ie without any 
#' reordering. If a vector of integers, then dendrogram is computed 
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
#' is issued and Rowv (or Colv) arguments are honoured.
#' 
#' @param reorderfun function(d, w) of dendrogram and weights for reordering 
#' the row and column dendrograms. The default uses stats{reorder.dendrogram}
#' 
#' @param k_row an integer scalar with the desired number of groups by which 
#' to color the dendrogram's branches in the rows 
#' (uses \link[dendextend]{color_branches})
#' 
#' @param k_col an integer scalar with the desired number of groups by which 
#' to color the dendrogram's branches in the columns 
#' (uses \link[dendextend]{color_branches})
#' @param print.values \emph{logical} Show the values inside the cells. Defatuls to \code{FALSE}.
#' @param show.legend Show color key and density
#'    information? \code{TRUE/FALSE}. Defaults to \code{FALSE}
#' @param legend.title Separate title for legend. Defaults to \code{NULL}.
#' @param legend.location \emph{Required, character} Location for legend, either \code{"fl", "br", "tr"
#' "tl", or "bl"} for "float", "bottom right", "top right", "top left", and "bottom left". 
#' Defaults to \code{"fl"}, which follows the location of the axis labels.
#' @param width Width in pixels (optional, defaults to automatic sizing).
#' @param height Height in pixels (optional, defaults to automatic sizing).
#' 
#' @param xaxis_height Size of axes, in pixels.
#' @param yaxis_width Size of axes, in pixels.
#' @param xaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
#' @param yaxis_font_size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
#' @param xaxis_angle Angle of axis labels. Defaults to 60. Maximum of 90 (vertical), minimum of 25.
#' @param xaxis_location Location of the x axis, either "bottom" or "top". Defaults to "bottom".
#' @param yaxis_location Location of the y axis, either "right" or "left". Defaults to "right".
#' @param xaxis_title Title text of x axis
#' @param yaxis_title Title text of y axis
#' @param xaxis_title_font_size Font size of x axis title in pixels. Defaults to 14.
#' @param yaxis_title_font_size Font size of y axis title in pixels. Defaults to 14. 
#' 
#' @param brush_color The base color to be used for the brush. The brush will be
#'   filled with a low-opacity version of this color. \code{"#RRGGBB"} format 
#'   expected.
#' @param show_grid \code{TRUE} to show gridlines, \code{FALSE} to hide them, or
#'   a numeric value to specify the gridline thickness in pixels (can be a non-integer).
#' @param anim_duration Number of milliseconds to animate zooming in and out.
#'   For large \code{x} it may help performance to set this value to \code{0}.
#'  
#' 
#' @param symm logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
#' @param revC logical indicating if the column order should be reversed for plotting.
#' Default (when missing) - is FALSE, unless symm is TRUE.
#' This is useful for cor matrix.
#' 
#' @param scale character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none".
#' @param na.rm logical indicating whether NA's should be removed.
#' @param na.value numeric indicating where NA's should be substituted to trigger the NA color.
#' @param na.color Color of NA values in heatmap. Defaults to neutral gray.
#' 
#' @param rng A vector of two numbers, namely the minimum and maximum value
#'   to use when determining the mapping from values to colors. This is useful
#'   when the range of values changes between heatmaps, but colors should be the
#'   same (optional, defaults to the minimum and maximum of \code{x}).
#'   
#' @param digits \emph{integer} indicating the number of decimal places to be used by \link{round} for 'label'.
#' @param cellnote_row \emph{character} Label to display next to the row value when the user hovers over the cell.
#'  If not specified, trys to match the \code{xaxis_title}; if no axis title, defaults to "Row".
#' @param cellnote_col \emph{character} Label to display next to the column value when the user hovers over the cell. 
#' If not specified, trys to match the \code{yaxis_title}; if no axis title, defaults to "Col".
#' @param cellnote_val \emph{character} Label to display next to the cell value when the user hovers over the cell.
#'  Defaults to "Value".
#' @param cellnote \emph{numeric} (optional) matrix of the same dimensions as \code{x} that has the human-readable version of each value, for displaying to the user on hover. If \code{NULL}, then \code{x} will be coerced using \code{\link{as.character}}.
#' If missing, it will use \code{x}, after rounding it based on the \code{digits} parameter.
#' @param cellnote_scale \emph{logical} (default is FALSE). IF cellnote is missing and x is used, 
#' should cellnote be scaled if x is also scaled?
#' 
#' @param cexRow positive numbers. If not missing, it will override \code{xaxis_font_size}
#' and will give it a value cexRow*14
#' @param cexCol positive numbers. If not missing, it will override \code{yaxis_font_size}
#' and will give it a value cexCol*14
#'
#' @param labRow character vectors with row labels to use (from top to bottom); default to rownames(x).
#' @param labCol character vectors with column labels to use (from left to right); default to colnames(x).
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
#' library(d3heatmap)
#' d3heatmap(mtcars, scale = "column", colors = "Blues")
#' 
#' 
d3heatmap <- function(x,
	, main = NULL
  , width = NULL,
  , height = NULL
  , show_grid = TRUE
  , anim_duration = 500
  , rng = NULL
  
	## dendrogram control
  , Rowv = TRUE
  , Colv = if (symm) "Rowv" else TRUE
  , distfun = dist
  , hclustfun = hclust
  , dendrogram = c("both", "row", "column", "none")
  , reorderfun = function(d, w) reorder(d, w)
  , k_row = NULL
  , k_col = NULL
  , symm = FALSE
  , revC = NULL
  
  ## data scaling
  , scale = c("none", "row", "column")
  , na.rm = TRUE
  , na.color = "#777777"
  , na.value = NA

  ## legend 
  , legend.title = NULL
  , show.legend = FALSE
  , legend.location = c("fl", "br", "tr", "tl", "tr"
	
  ## cellnote formatting
  , digits = 3L
  , cellnote = NULL
  , cellnote_scale = FALSE
  , cellnote_row = NULL
  , cellnote_col = NULL,
  , cellnote_val = "Value",
  , brush_color = "#0000FF"
	, print.values = FALSE

	## color controls  
  , theme = NULL
  , colors = "RdYlBu"
  , bins = NULL
  , symmetrical = FALSE
	
	## axis controls
  , labRow = rownames(x),
  , labCol = colnames(x),
  , xaxis_height = 80
  , yaxis_width = 120
  , xaxis_font_size = NULL
  , yaxis_font_size = NULL
  , xaxis_angle = 60
  , xaxis_location = "bottom"
  , yaxis_location = "right"
  , xaxis_title = NULL
  , yaxis_title = NULL
  , xaxis_title_font_size = 14
  , yaxis_title_font_size = 14

	## deprecate these
  , cexRow
  , cexCol

) {
 
  scale <- match.arg(scale)
   
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
		, dendrogram = dendrogram
		, reorderfun = reorderfun
		, k_row = k_row
		, k_col = k_col
		, symm = symm
		, revC = revC
		, scale = scale
		, na.rm = na.rm
		, na.value = na.value
		, digits = digits
		, cellnote = cellnote
		, cellnote_scale = cellnote_scale
		, labrow = labRow
		, labCol = labCol
	)
	
  
	## the big call to create the heatmap
  ##==============================
	## inserting new call to the heatmap function, which does everything up to 
	## this point that d3heatmap did
	hm <- do.call(heatmap, args = params)

	x <- hm$x
	
	## Colors for the heatmap and the legend
  ##===========================================
  if (is.factor(x)) {
    legend_colors <- scales::col_bin(colors, domain = 1:length(factor(x)), 
                                     na.color = na.color)(1:length(factor(x)))
    colors <- scales::col_factor(colors, x, na.color = na.color)
    
  } else {
    if (is.null(rng)) {
          rng <- range(x, na.rm = TRUE)
          if (scale %in% c("row", "column")) {
              rng <- c(max(abs(rng)), -max(abs(rng)))
          }
    }
  
    if(is.null(bins)) {
      bins <- 50
      colors <- scales::col_numeric(colors, rng, na.color = na.color)
    } else {
      colors <- scales::col_bin(colors, rng, bins = bins, na.color = na.color)
    }
    legend_colors <- scales::col_bin(colors, domain = 1:bins, bins = bins, na.color = na.color)(1:bins)
  }
  

	## axis and cellnote labels sync
  ##==============================
  if(is.null(cellnote_row)) cellnote_row <- yaxis_title
  if(is.null(cellnote_col)) cellnote_col <- xaxis_title

	## bundling the options
  ##==============================================
	xaxis_angle <- min(90, max(xaxis_angle, 25))
	legend.location <- match.arg(legend.location)

  options <- list(
    hm_width = width
    , hm_height = height
    , xaxis_height = xaxis_height
    , yaxis_width = yaxis_width
    , xaxis_font_size = xaxis_font_size
    , yaxis_font_size = yaxis_font_size
    , xaxis_angle = xaxis_angle
    , xaxis_location = xaxis_location
    , yaxis_location = yaxis_location
    , xaxis_title = xaxis_title
    , yaxis_title = yaxis_title
    , xaxis_title_font_size = xaxis_title_font_size
    , yaxis_title_font_size = yaxis_title_font_size 
    , bins = bins
    , symmetrical = symmetrical
    , print_values = print.values
    , show_legend = show.legend
    , legend_title = legend.title
    , legend_location = legend.location
		, legend_colors = legend_colors
    , brush_color = brush_color
    , na_color = na.color
    , show_grid = show_grid
    , cellnote_row = cellnote_row
    , cellnote_col = cellnote_col
    , cellnote_val = cellnote_val
    , anim_duration = anim_duration
  )

  if (is.null(hm$rowDend)) c(options, list(yclust_width = 0))
  if (is.null(hm$colDend)) c(options, list(xclust_height = 0))
 
 	## proceed to the widget
	##=======================================	
  imgUri <- encodeAsPNG(t(x), colors)
	
  payload <- list(
    rows = hm$rowDend 
		, cols = hm$colDend
		, matrix = hm$mtx
		, title = main
		, image = imgUri
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

