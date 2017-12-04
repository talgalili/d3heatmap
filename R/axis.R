#' Modify axis options
#' 
#' Provides options for modifying the x and y axes of a D3 Heatmap widget
#' 
#' @param heatmap a d3heatmap object created from the d3heatmap() or hmHeatmap()
#' @param axis Name of the axis to modify; either "x", "y", "row", or "column"
#' @param size Size of axes, in pixels.
#' @param labels character vectors with axis labels to use (top to bottom for y axis, left to right for x); default to rownames(x) or colnames(x).
#' @param font.size Font size of axis labels, as a CSS size (e.g. "14px" or "12pt").
#' @param angle Angle of x axis labels (x axis only). Defaults to 60. Maximum of 90 (vertical), minimum of 25.
#' @param location Location of the axis, either "bottom" or "top" for the x axis, and 
#' either "right" or "left" for the y axis. Defaults to "bottom" and "right".
#' @param title Title text
#' @param title.font.size Font size of axis title in pixels. Defaults to 14.
#' 
#' @export
hmAxis <- function(heatmap
  , axis = c("x", "y", "row", "column")
  , size = NULL
  , labels
  , font.size = NULL
  , angle = NULL
  , location = c("bottom", "top", "right", "left")
  , title = NULL
  , title.font.size = NULL
) {
  
  if(missing(axis)) {
    message("hmAxis: no axis specified... returning original heatmap")
    return(heatmap)
  }
  
  x <- heatmap$x$matrix
  options <- heatmap$x$options
  
  axis <- match.arg(axis)
  location <- match.arg(location)
  
  if(location %in% c("bottom", "top")) xaxis_location <- location
  else xaxis_location <- NULL
  
  if(location %in% c("left", "right")) yaxis_location <- location
  else yaxis_location <- NULL
  
  if (axis %in% c("x", "column")) {
    if(!missing(labels)) colnames(x) <- labels
   
    cellnote_col <- heatmap$x$options$cellnote_col 
    if(is.null(cellnote_col)) cellnote_col <- 
	  
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
    if(!missing(labels)) rownames(x) <- labels
    
    cellnote_row <- heatmap$x$options$cellnote_row 
    if(is.null(cellnote_row)) cellnote_row <- title
  
    opts <- list(
      yaxis_width = size,
      yaxis_font_size = font.size,
      yaxis_location = yaxis_location, 
      yaxis_title = title,
      yaxis_title_font_size = title.font.size,
      cellnote_row = cellnote_row
    )
  
  } else return(heatmap)
  
  x <- heatmap$x
  x$options <- mergeLists(options, opts)
  rm(heatmap)
  
  # re-create widget
  # I'm not a big fan of having to recreate the widget, but since the axis options affect the 
  # sizing of the grid elements in the heatmap, this is the simpler solution. Merely pasting in options
  # causes sizing and placement issues
  htmlwidgets::createWidget(
    name = 'd3heatmap',
    x = x,
    width = x$hm_width,
    height = x$hm_height,
    package = 'd3heatmap',
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
  )
}