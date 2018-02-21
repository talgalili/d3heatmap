#' Modify d3heatmap options
#' 
#' Change options for a d3heatmap
#' 
#' @param d3heatmap a d3heatmap object created from the d3heatmap()
#' 
#' @param title \emph{string} Plot title. Defaults to \code{NULL}.
#' 
#' @param scale character indicating if the values should be centered and scaled in either 
#' the row direction or the column direction, or none. The default is "none".
#'
#' @param scale.by.range logical indicating whether to scale rows or columns by
#' the range of each row and column. Setting this parameter to \code{TRUE} 
#' automatically sets \code{na.rm} to \code{TRUE}
#' 
#' @param show.grid \code{TRUE} to show gridlines, \code{FALSE} to hide them, 
#' or a numeric value to specify the gridline thickness in pixels (can 
#' be a non-integer).
#' 
#' @param na.rm logical indicating whether NA's should be removed.
#' 
#' @param na.value numeric indicating where NA's should be substituted to trigger the NA color.
#' 
#' @param animation.duration Number of milliseconds to animate zooming in and 
#' out. For large \code{x} it may help performance to set this value to 
#' \code{0}.
#'  
#' @param reverse.columns logical indicating if the column order should be 
#' reversed for plotting. Default (when missing) - is FALSE, unless symmetrical 
#' is TRUE... useful for correlation matrices.
#' 
#' @return Modified d3heatmap object
#' 
#' @source 
#' The interface was inspired by \cite{dygraphs}
#' 
#' @seealso 
#' \link{heatmap}, \link[gplots]{heatmap.2}
#' 
#' @examples 
#' \dontrun{
#' 
#' d3heatmap(mtcars, scale = "column", col = "Blues") %>%
#'   hmOptions(scale.by.range = TRUE, show.grid = FALSE,
#'	 		animation.duration = 400)
#' 
#' }
#' 
#' @export
hmOptions <- function(d3heatmap
	, title #main
  , scale = c("none", "row", "column") #scale
	, scale.by.range #scale.by.range
	, na.rm #na.rm
	, na.value #na.value
	, show.grid #show_grid
	, animation.duration #anim_duration
	, reverse.columns) {
  
	if (missing(d3heatmap))
		message("hmOptions: no d3heatmap provided")
  
	params <- d3heatmap$x$params
  x <- params$x
  options <- d3heatmap$x$options

	new.opts <- list()
	new.params <- list()

	## process parameters
  ##==============================================
	if(!missing(scale)) new.params$scale <- match.arg(scale)
	if(!missing(scale.by.range)) new.params$scale.by.range <- scale.by.range
	if(!missing(na.rm)) new.params$na.rm <- na.rm
	if(!missing(na.value)) new.params$na.value <- na.value
	if(!missing(reverse.columns)) new.params$revC <- reverse.columns
	
	if(!missing(show.grid)) new.opts$show_grid <- show.grid
	if(!missing(animation.duration)) 
					new.opts$anim_duration <- animation.duration

  ## call heatmap with the updated params and save
  ## the params with the heatmap for later use
  ##==============================================
	params <- mergeLists(params, new.params)

  hm <- do.call(heatmap, args = params)
	x <- hm$x
	 
  ## process re-coloring	
  ##==============================================
	hm_colors <- heatmapColors(x
								, params$col
								, params$na.color
								, params$na.rm
								, params$rng
								, params$scale
								, params$breaks
								, params$symbreaks
							)
  imgUri <- encodeAsPNG(t(x), hm_colors$col)

	## load up the widget	
  ##==============================================
	if(!missing(title)) d3heatmap$x$title <- title
	d3heatmap$x$rows <- hm$rowDend
	d3heatmap$x$cols <- hm$colDend
	d3heatmap$x$matrix <- hm$mtx
  d3heatmap$x$image <- imgUri
	d3heatmap$x$params <- params
	
  options <- mergeLists(options, new.opts)
	d3heatmap$x$options <- options

  return(d3heatmap)  
}

