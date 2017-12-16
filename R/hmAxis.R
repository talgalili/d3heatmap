#' Modify axis options
#' 
#' Provides options for modifying the x and y axes of a D3 Heatmap widget
#' 
#' @param heatmap a d3heatmap object created from the d3heatmap() or hmHeatmap()
#' 
#' @param axis Name of the axis to modify; either "x", "y", "row", or "column"
#' 
#' @param size Size of axes, in pixels.
#' 
#' @param labels character vectors with axis labels to use (top to bottom for y axis, left to right for x); default to rownames(x) or colnames(x).
#' 
#' @param font.size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
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
#' @export
hmAxis <- function(d3heatmap
  , axis = c("x", "y", "row", "column")
  , size = NULL
  , labels
  , font.size = NULL
  , angle = NULL
  , location
  , title = NULL
  , title.font.size = NULL
) {
  
  if(missing(axis)) {
    message("hmAxis: no axis specified... returning original heatmap")
    return(d3heatmap)
  }
 
	params <- d3heatmap$x$params
  x <- params$x
  options <- d3heatmap$x$options
  
  axis <- match.arg(axis)
  
  if (axis %in% c("x", "column")) {
    
    if(!missing(location)) xaxis_location <- match.arg(location, c('bottom', 'top'))
    else xaxis_location <- options$xaxis_location
  
    if(!missing(labels)) colnames(x) <- labels
   
    cellnote_col <- options$cellnote_col 
    if(is.null(cellnote_col)) cellnote_col <- title
	  
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
    
    if(!missing(location)) yaxis_location <- match.arg(location, c('right', 'left'))
    else yaxis_location <- options$yaxis_location
    
    if(!missing(labels)) rownames(x) <- labels
    
    cellnote_row <- options$cellnote_row 
    if(is.null(cellnote_row)) cellnote_row <- title
  
    opts <- list(
      yaxis_width = size,
      yaxis_font_size = font.size,
      yaxis_location = yaxis_location, 
      yaxis_title = title,
      yaxis_title_font_size = title.font.size,
      cellnote_row = cellnote_row
    )
  
  } else return(d3heatmap)
  
  params$x <- x
	
	## call heatmap with the updated params and save
	## the params with the heatmap for later use
  ##==============================================
	hm <- do.call(heatmap, args = params)
  options <- mergeLists(options, opts)

	new <- list(
		matrix = hm$mtx
		, params = params
		, options = options
	)

	d3heatmap$x <- mergeLists(d3heatmap$x, new, recursive = F)
  return(d3heatmap)  
}

