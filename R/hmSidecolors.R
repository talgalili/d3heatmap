#' Modify axis options
#' 
#' Provides options for modifying the x and y axes of a D3 Heatmap widget
#' 
#' @param d3heatmap a d3heatmap object created from the d3heatmap() or 
#' hmHeatmap()
#' 
#' @param axis Name of the axis to modify; either "x", "y", "row", or "column"
#' 
#' @param side.colors character vector of length ncol(x), or matrix with
#' columns equal to ncol(x), containing the color names for a horizontal 
#' side bar that may 
#' be used to annotate the rows or columns of x.
#'   
#' @param palette a palette of colors to use for side colors if passing a 
#' non-color matrix
#'   
#' @param names the names of the variables for the side colors.
#'   
#' @examples 
#' library(d3heatmap)
#' library(magrittr)
#' 
#' rsc <- matrix(rep_len(c('good', 'bad', 'ugly'), length.out = 64), ncol = 2)
#' rsccols <- c('red', 'white', 'blue')
#' rscnames <- c('Row 1', 'Row 2')
#' 
#' csc <- matrix(rep_len(c('first', 'second', 'third', 'fourth', 'fifth'), length.out = 33), nrow = 3)
#' csccols <- c('orange', 'blue', 'grey', 'green', 'red')
#' cscnames <- c('Column 1', 'Column 2', 'Column 3')
#' 
#' d3heatmap(mtcars, key = TRUE, scale = 'column') %>% 
#'   hmSideColors(axis = 'column', side.colors = csc,
#'     palette = csccols, names = cscnames) %>% 
#'   hmSideColors(axis = 'y', side.colors = rsc,
#'     palette = rsccols, names = rscnames)
#' 
#' 
#' @source 
#' The interface was inspired by \cite{dygraphs}
#' 
#' @seealso 
#' \link{heatmap}, \link[gplots]{heatmap.2}
#' 
#' @export
hmSideColors <- function(d3heatmap
  , axis = c("x", "y", "row", "column")
	, side.colors
  , palette
  , names
){
  
	if (missing(d3heatmap)) {
		message("hmLegend: no heatmap provided... returning NULL")
		return(NULL)
	}
  
  if (missing(axis)) {
    message("hmSideColors: no axis specified... returning original heatmap")
    return(d3heatmap)
  }
  
	params <- d3heatmap$x$params
  options <- d3heatmap$x$options
	new <- list()
	new.opts <- list()

  axis <- match.arg(axis)
  
  if (axis %in% c("y", "row")) {
		if(!missing(side.colors)) new$RowSideColors <- side.colors
		if(!missing(palette)) new$RowColorsPalette <- palette
		if(!missing(names)) new.opts$rsc_colnames <- names
  
	} else if (axis %in% c("x", "column")) {
		if(!missing(side.colors)) new$ColSideColors <- side.colors
		if(!missing(palette)) new$ColColorsPalette <- palette
		if(!missing(names)) new.opts$csc_colnames <- names

	}

	params <- mergeLists(params, new)

	## call heatmap with the updated params and save
	## the params with the heatmap for later use
  ##==============================================
	hm <- do.call(heatmap, args = params)

	## save heatmap outputs into options for loading
  ##==============================================
	new.opts$rsc_cols <- hm$rsccols
	new.opts$csc_cols <- hm$csccols
	new.opts$rsc_labs <- hm$rsclabs
	new.opts$csc_labs <- hm$csclabs

	options <- mergeLists(options, new.opts)
	
	# transpose throws error if object is null
	if(!is.null(hm$rowcolors)) hm$rowcolors <- t(hm$rowcolors)

	## Load up all the new structs into the widget	
  ##==============================================
	d3heatmap$x$params <- params
	d3heatmap$x$rowcolors <- hm$rowcolors
	d3heatmap$x$colcolors <- hm$colcolors
	d3heatmap$x$options <- options

  return(d3heatmap)  
}
