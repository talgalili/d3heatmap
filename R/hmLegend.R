#' Modify the d3heatmap legend
#' 
#' Provides options for modifying the legend for a d3heatmap object
#' 
#' @param show \emph{Required, logical} Show color key and density
#'    information? \code{TRUE/FALSE}. Defaults to \code{FALSE}
#' @param title \emph{Optional, character} Separate title for legend. 
#' Defaults to \code{NULL}.
#' @param location \emph{Required, character} Location for legend, either 
#' \code{"fl", "br", "tr", "tl", or "bl"} for "float", "bottom right", 
#' "top right", "top left", and "bottom left". Defaults to \code{"fl"}, 
#' which follows the location of the axis labels.
#' 
#' @return Modified d3heatmap object
#' @export
hmLegend <- function(heatmap
  , show = FALSE
  , title = NULL
  , location = c("fl", "br", "tr", "tl", "bl")
) {
  
  if(missing(heatmap)) {
		message("hmLegend: no heatmap provided... returning NULL")
		return(NULL)
	}
  
  location <- match.arg(location)
  
  opts <- list(
    show_legend = show
    , legend_title = title
    , legend_location = location
  )
  
  heatmap$x$options <- mergeLists(heatmap$x$options, opts)
  return(heatmap) 
}

