#' Modify the d3heatmap legend
#' 
#' Provides options for modifying the legend for a d3heatmap object
#' 
#' @param show \emph{Required, logical} Show color key and density
#'    information? \code{TRUE/FALSE}. Defaults to \code{FALSE}
#' 
#' @param size \emph{numeric} value indicating the relative size of the key. 
#' Will multiple the row and column axis sizes. Defaults to 1
#' 
#' @param title \emph{Optional, character} Separate title for color key (legend). 
#' Defaults to \code{NULL}.
#' 
#' @param density \emph{Optional, character} indicating whether to superimpose 
#' a 'histogram' or no plot ('none') on the color-key (legend). 
#' 
#' @param density.fill \emph{Optional, character} giving the color for the density 
#' display specified by 'density', defaults to \code{NULL} which will fill the
#' bars with the same color as the bin
#' 
#' @param location \emph{Required, character} Location for legend (color key), either 
#' \code{"fl", "br", "tr", "tl", or "bl"} for "float", "bottom right", 
#' "top right", "top left", and "bottom left". Defaults to \code{"fl"}, 
#' which follows the location of the axis labels.
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
#' library(magrittr)
#' 
#' d3heatmap(mtcars, dendrogram = 'none', scale = 'column', srtCol = 30) %>% 
#'   hmLegend(show = T, title = "Title", location = "tl") %>% 
#'   
#' @export
hmLegend <- function(heatmap
  , show = TRUE
  , size
  , title
  , location = c("fl", "br", "tr", "tl", "bl")
  , density = c('histogram', 'none')
  , density.fill = NULL
) {
  
  if (missing(heatmap)) {
		message("hmLegend: no heatmap provided... returning NULL")
		return(NULL)
  }
  
  if(missing(size)) size <- heatmap$x$options$legend_scaler
  if(missing(title)) title <- heatmap$x$options$legend_title
  
  opts <- list(
    show_legend = show
    , legend_scaler = size
    , legend_title = title
    , legend_location = match.arg(location)
    , legend_type = match.arg(density)
    , legend_fill = density.fill
  )
  
  heatmap$x$options <- mergeLists(heatmap$x$options, opts)
  return(heatmap) 
}

