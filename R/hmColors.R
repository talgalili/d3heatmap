#' @import htmlwidgets
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats as.dendrogram dendrapply dist hclust is.leaf order.dendrogram reorder sd
NULL


#' Set d3heatmap color options
#' 
#' Set and adjust the colors and color options for the d3heatmap
#' 
#' @param heatmap \emph{Required} A valid \emph{d3heatmap} object
#' 
#' @param theme A custom CSS theme to use. Currently the only valid values 
#' are \code{""} and \code{"dark"}. \code{"dark"} is primarily intended for 
#'   standalone visualizations, not R Markdown or Shiny.
#' 
#' @param colors Either a colorbrewer2.org palette name (e.g. 
#' \code{"YlOrRd"} or \code{"Blues"}), or a vector of colors 
#' to interpolate in hexadecimal \code{"#RRGGBB"} format, or a color 
#' interpolation function like \code{\link[grDevices]{colorRamp}}.
#' 
#' @param bins \emph{integer} The number of colors to generate from 
#' the palette.
#' @param symbreaks \emph{logical} Arrange color bins symmetrically around 
#' zero?
#' 
#' @param na.color Color of NA values in heatmap. Defaults to neutral gray.
#' 
#' @param rng A vector of two numbers, namely the minimum and maximum value
#'   to use when determining the mapping from values to colors. This is 
#' useful when the range of values changes between heatmaps, but colors 
#' should be the same (optional, defaults to the minimum and maximum 
#' of \code{x}).
#'   
#' @import htmlwidgets
#'   
#' @export
#' @source 
#' The interface was inspired by \link{dygraphs}
#'   
hmColors <- function(heatmap
  , theme = c('', 'dark')
  , colors
  , range
  , color.bins
  , symmetrical # symbreaks
  , na.color
)
{
  
	if(missing(heatmap)) {
    message("hmColors: no heatmap provided... returning NULL")
    return(NULL)
  }

	## grab original and modified parameters to feed the color creation
	params <- heatmap$params
	x <- heatmap$x

	## process new parameters passed
	if(missing(colors)) colors <- params$colors
	if(missing(range)) range <- params$rng
	if(missing(color.bins)) color.bins <- params$bins
	if(missing(symmetrical)) symmetrical <- params$symbreaks
	if(missing(na.color)) na.color <- params$na.color

	new <- list(
		colors = colors
		, rng = range
		, bins = color.bins
		, symbreaks = symmetrical
		, na.color = na.color
	)

	params <- mergeLists(params, new)
	
	hm <- do.call(heatmap, args = params)
	x <- hm$x
	
	## Colors for the heatmap and the legend
  ##===========================================
	hm_colors <- heatmapColors(x
								, params$colors
								, params$na.color
								, params$na.rm
								, params$rng
								, params$scale
								, params$bins
							)

	if(missing(theme)) theme <- heatmap$theme
	else theme <- match.arg(theme)
	
	## proceed to the widget
	##=======================================	
  imgUri <- encodeAsPNG(t(x), hm_colors$colors)

	newOpts <- list(
		legend_colors = hm_colors$legend_colors
		, bins = hm_colors$bins
		, na_color = params$na.color
	)
	options <- mergeLists(heatmap$options, newOpts)
  
	payload <- list(
    rows = hm$rowDend 
		, cols = hm$colDend
		, matrix = hm$mtx
		, image = imgUri
    , theme = theme 
		, options = options
		, params = params
	)

	heatmap$x <- payload

	return(heatmap)
}



