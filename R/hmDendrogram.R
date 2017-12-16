#' Set d3heatmap dendrograms
#' 
#' Set and adjust the dendrograms for a d3heatmap object
#' 
#' @param heatmap \emph{Required} A valid \emph{d3heatmap} object
#'
#' @param dendrogram \emph{Required} The dendrogram to process in this
#' call, among \code{'row', 'col', 'both', 'none'}. Using the \code{'none'}
#' value turns off any previous dendrogram settings.
#' 
#' @param dendrogram \emph{Required} A character string indicating whether 
#' to draw 'none', 'row', 'column' or 'both' dendrograms. 
#' Defaults to 'both'. However, if reorder or row.reorder and col.reorder 
#' are FALSE or NULL and dendrogram is 'both', then a warning is 
#' issued and row.reorder (or col.reorder) arguments are honoured.
#'
#' @param reorder a parameter that allows the user to pass in a single 
#' reordering value to use on the dendrograms specified in the 
#' \code{dendrogram} argument.
#'
#' @param row.reorder determines if and how the row dendrogram 
#' should be reordered.	By default, it is TRUE, which implies 
#' dendrogram is computed and reordered based on row means. 
#' If NULL or FALSE, then no dendrogram is computed and no reordering 
#' is done. If a dendrogram, then it is used "as-is", ie without any 
#' reordering. If a vector of integers, then dendrogram is computed 
#' and reordered based on the order of the vector.
#' 
#' @param col.reorder determines if and how the column dendrogram should be 
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
#' @param reorderfun function(d, w) of dendrogram and weights for reordering 
#' the row and column dendrograms. 
#'
#' @param reverse.columns logical indicating if the column order should be 
#' reversed for plotting. Default (when missing) is FALSE, unless symmetrical
#' is TRUE. This is useful for correlation matrices.
#'   
#' @import htmlwidgets
#'   
#' @source 
#' The interface was inspired by \link{dygraphs}
#'   
#' @examples 
#' library(d3heatmap)
#' library(dplyr)
#' d3heatmap(mtcars, scale = "column", colors = "Blues") %>%
#' hmDendrogram(dendrogram = 'row', row.groups = 3)
#' 
#' @export
hmDendrogram <- function(heatmap
	## dendrogram control
  , dendrogram = c('row', 'column', 'both', 'none')
	, reorder #Rowv & Colv
  , row.reorder #Rowv
  , column.reorder #Colv
  , distance.function
  , hclustfun #clustering.function
  , reorderfun #reorder.function
  , row.groups #row.groups
  , column.groups #column.groups
  , symmetrical #symmetrical
  , reverse.columns #reverse.columns
)
{

	# perform critical argument checks
	if(missing(heatmap)) {
		message('hmDendrogram: no heatmap provided, returning NULL')
		return(NULL)
	}

	if(missing(dendrogram)) {
		message('hmDendrogram: no dendrogram specified... returning original 
						heatmap')
		return(heatmap)
	}
 
	# grab the input parameters for generating the heatmap
	params <- heatmap$x$params

  dendrogram <- match.arg(dendrogram)
	
	# if setting to 'none', then we're turning off past
	# dendrogram arguments, and we'll call heatmap() and exit early
	if (dendrogram == 'none') {
		params$dendrogram <- 'none'
		hm <- do.call(heatmap, args = params)
		return(hm)
	}
	
	# setup some control logicals
	row <- (dendrogram %in% c('row', 'both'))
	col <- (dendrogram %in% c('col', 'both'))

	# combining old setting with new setting	
	both <- (params$dendrogram	== 'col' & row) |
					(params$dendrogram == 'row' & col) |
					(params$dendrogram == 'both')

	# perform operations on each possible dendrogram
	# NOTE: if supplied, the dimension-specific (e.g., row.order)
	# takes precedence over the generic one (i.e., reorder)	

	# TODO: control for when only one dendrogram is being processed, 
	# don't change the other
	if(missing(reorder)) reorder <- TRUE

	if (row) {
		if (missing(row.reorder)) row.reorder <- reorder
		dendrogram <- 'row'
	}

	if (col) {
		if (missing(column.reorder)) column.reorder <- reorder
		dendrogram <- 'col'

		if(symmetrical) column.reorder <- row.reorder
	}

	if (both) dendrogram <- 'both'

	# process other arguments
	if(missing(distance.function)) distance.function <- params$distfun
	if(missing(row.groups)) row.groups <- params$k_row
	if(missing(column.groups)) column.groups <- params$k_col
	if(missing(symmetrical)) symmetrical <- params$symm
	if(missing(reverse.columns)) reverse.columns <- params$revC

  new <- list(
			dendrogram = dendrogram
			, Rowv = row.reorder
			, Colv = column.reorder
			, distfun = distance.function
			, hclustfun = hclustfun
			, dendrogram = dendrogram
			, reorderfun = reorderfun
			, k_row = row.groups
			, k_col = column.groups
			, symm = symmetrical
			, revC = reverse.columns
	)

	params <- mergeLists(params, new)

	## call heatmap with the updated params and save
	## the params with the heatmap for later use
  ##==============================================
	hm <- do.call(heatmap, args = params)
	hm$x$params <- params

	return(hm)
}

