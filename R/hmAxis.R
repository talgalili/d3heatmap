#' Modify axis options
#' 
#' Provides options for modifying the x and y axes of a D3 Heatmap widget
#' 
#' @param d3heatmap a d3heatmap object created from the d3heatmap() or hmHeatmap()
#' 
#' @param axis Name of the axis to modify; either "x", "y", "row", or "column"
#' 
#' @param size Size of axes, in pixels.
#' 
#' @param labels character vectors with axis labels to use (top to bottom for y axis, left to right for x); default to rownames(x) or colnames(x).
#' 
#' @param font.size integer Font size of axis labels, in pixels (i.e., will be translated to a 
#' character string with 'px' appended)
#' 
#' @param angle Angle of x axis labels (x axis only). Defaults to 60. Maximum of 90 (vertical), minimum of 25.
#' 
#' @param location Location of the axis, either "bottom" or "top" for the x axis, and 
#' either "right" or "left" for the y axis. Defaults to "bottom" and "right".
#' 
#' @param title Title text
#' 
#' @param title.font.size Font size of axis title in pixels. Defaults to 14.
#'   
#' @return Modified d3heatmap object
#' 
#' @import htmlwidgets
#'   
#' @source 
#' The interface was inspired by \link{dygraphs}
#' 
#' @seealso 
#' \link{heatmap}, \link[gplots]{heatmap.2}
#' 
#' @examples 
#' library(d3heatmap)
#' library(dplyr)
#' d3heatmap(mtcars, scale = "column", col = "Blues") %>%
#'   hmAxis("x", angle = 30, title = "test", location = 'top', font.size = '24px') %>% 
#'   hmAxis("y", title = "test", location = 'right')
#' 
#' @export
hmAxis <- function(d3heatmap
  , axis = c("x", "y", "row", "column")
  , size
  , labels
  , font.size
  , angle
  , location
  , title
  , title.font.size
) {
  
	if (missing(d3heatmap)) {
		message("hmLegend: no heatmap provided... returning NULL")
		return(NULL)
	}
  
  if (missing(axis)) {
    message("hmAxis: no axis specified... returning original heatmap")
    return(d3heatmap)
  }
 
	params <- d3heatmap$x$params
  x <- params$x
  options <- d3heatmap$x$options
  
  axis <- match.arg(axis)
  
  if (axis %in% c("x", "column")) {
   
    if (missing(size)) size <- options$xaxis_height 
    if (missing(title)) title <- options$xaxis_title
    if (missing(title.font.size)) title.font.size <- options$xaxis_title_font_size
    
    if (!missing(location)) xaxis_location <- match.arg(location, c('bottom', 'top'))
    else xaxis_location <- options$xaxis_location
  
    if (missing(font.size)) font.size <- options$xaxis_font_size
    if (!missing(labels)) colnames(x) <- labels
   
    cellnote_col <- options$cellnote_col 
    if (is.null(cellnote_col)) cellnote_col <- title
  
    if (missing(angle)) angle <- options$srtCol 
    angle <- min(90, max(angle, 25))
    
    opts <- list(
      xaxis_height = size,
      xaxis_font_size = font.size,
      xaxis_angle = angle,
      xaxis_location = xaxis_location, 
      xaxis_title = title,
      xaxis_title_font_size = title.font.size,
      cellnote_col = cellnote_col
    )
    
  } else if (axis %in% c("y", "row")) {
    if (missing(size)) size <- options$yaxis_width
    if (missing(title)) title <- options$yaxis_title
    if (missing(title.font.size)) title.font.size <- options$yaxis_title_font_size
    
    if (missing(font.size)) font.size <- options$yaxis_font_size
    
    if (!missing(location)) yaxis_location <- match.arg(location, c('right', 'left'))
    else yaxis_location <- options$yaxis_location
    
    if (!missing(labels)) rownames(x) <- labels
    
    cellnote_row <- options$cellnote_row 
    if (is.null(cellnote_row)) cellnote_row <- title
  
    opts <- list(
      yaxis_width = size,
      yaxis_font_size = font.size,
      yaxis_location = yaxis_location, 
      yaxis_title = title,
      yaxis_title_font_size = title.font.size,
      cellnote_row = cellnote_row
    )
  
  } else return(d3heatmap)

  # we only have to re-run the heatmap creation if we've 
  # changed the row or columns names for the matrix 
  if (missing(labels)) {
    params$x <- x
  	
    ## call heatmap with the updated params and save
  	## the params with the heatmap for later use
    ##==============================================
  	hm <- do.call(heatmap, args = params)
	  d3heatmap$x$matrix <- hm$mtx
	  d3heatmap$x$params <- params
  }
	
  options <- mergeLists(options, opts)

	d3heatmap$x$options <- options

  return(d3heatmap)  
}

