#' @import htmlwidgets
NULL


#'   
#' Set d3heatmap dendrograms
#' 
#' Set and adjust the dendrograms for a d3heatmap object
#' 
#' @param d3heatmap \emph{Required} A valid \emph{d3heatmap} object
#'
#' @param dendrogram \emph{Required} The dendrogram to process in this
#' call, among \code{'row', 'col', 'both', 'none'}. Using the \code{'none'}
#' value turns off any previous dendrogram settings. If reorder or row.reorder 
#' and col.reorder are FALSE or NULL and dendrogram is 'both', then a warning 
#' is issued and row.reorder (or col.reorder) arguments are honored.
#' 
#' @param reorder a parameter that allows the user to pass in a single 
#' reordering value to use on the dendrograms specified in the 
#' \code{dendrogram} argument.
#'
#' @param row.reorder determines if and how the row dendrogram 
#' should be reordered.	By default, it is TRUE, which implies 
#' dendrogram is computed and reordered based on row means. 
#' If NULL or FALSE, then no dendrogram is computed and no reordering 
#' is done. If a dendrogram, then it is used "as-is", i.e., without any 
#' reordering. If a vector of integers, then dendrogram is computed 
#' and reordered based on the order of the vector.
#' 
#' @param column.reorder determines if and how the column dendrogram should be 
#' be reordered.	Has the options as the Rowv argument above and additionally 
#' when x is a square matrix, Colv = "Rowv" means that columns should be 
#' treated identically to the rows.
#' 
#' @param distance.function function used to compute the distance 
#' (dissimilarity) between both rows and columns. Defaults to dist.
#' 
#' @param clustering.function function used to compute the hierarchical 
#' clustering when Rowv or Colv are not dendrograms. Defaults to hclust.
#' 
#' @param reorder.function function(d, w) of dendrogram and weights for 
#' reordering the row and column dendrograms. 
#'
#' @param groups an integer scalar with the desired number of groups by which 
#' to color the dendrogram's branches (uses \link[dendextend]{color_branches})
#'   
#' @param symmetrical logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
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
#'   hmDendrogram(dendrogram = 'row', groups = 3)
#' 
#' }
#' 
#' @export
hmDendrogram <- function(d3heatmap
	## dendrogram control
  , dendrogram = c('row', 'column', 'both', 'none')
	, reorder
  , row.reorder #row.reorder
  , column.reorder #col.reorder
  , distance.function #distfun
  , clustering.function #hclustfun
  , reorder.function #reorder
  , groups 
  , symmetrical) {

	# perform critical argument checks
  ##==============================================
	if(missing(d3heatmap)) 
		stop('hmDendrogram: no d3heatmap provided')

	if(missing(dendrogram)) {
		message('hmDendrogram: no dendrogram specified... returning original 
						heatmap')
		return(d3heatmap)
	}
 
	# grab the input parameters for generating the heatmap
  ##==============================================
	params <- d3heatmap$x$params

	## process parameters
  ##==============================================
  dendrogram <- match.arg(dendrogram)
	
	# if setting to 'none', then we're turning off past
	# dendrogram arguments, and we'll call heatmap() and exit early
	if (dendrogram == 'none') {
		params$dendrogram <- 'none'
		hm <- do.call(heatmap, args = params)
	  x <- hm$x
	    
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
	  
	  d3heatmap$x$rows <- hm$rowDend
	  d3heatmap$x$cols <- hm$colDend
	  d3heatmap$x$matrix <- hm$mtx
    d3heatmap$x$image <- imgUri
	  d3heatmap$x$params <- params

  	return(d3heatmap)  
	}

	column.groups <- row.groups <- NULL
	
	# setup some control logicals
	row <- (dendrogram %in% c('row', 'both'))
	col <- (dendrogram %in% c('column', 'both'))

	# combining old setting with new setting	
	both <- (params$dendrogram	== 'col' & row) |
					(params$dendrogram == 'row' & col) |
					(params$dendrogram == 'both')

	if(missing(symmetrical)) symmetrical <- params$symm

	if (row) {
		if(missing(reorder)) reorder <- params$Rowv
		if(missing(row.reorder)) row.reorder <- reorder
		if(!missing(groups)) row.groups <- groups
		
		if(!col) column.reorder <- params$Colv
		
	}

	if (col) {
		if(missing(reorder)) reorder <- params$Colv
		if(missing(column.reorder)) column.reorder <- reorder
		if(!missing(groups)) column.groups <- groups

		if(symmetrical) column.reorder <- row.reorder
		if(!row) row.reorder <- params$Rowv
	}

	if (both) dendrogram <- 'both'

	# process other arguments
	if(missing(distance.function)) distance.function <- params$distfun
	if(missing(clustering.function)) clustering.function <- params$hclustfun
	if(missing(reorder.function)) reorder.function <- params$reorderfun
	if(is.null(row.groups)) row.groups <- params$k_row
	if(is.null(column.groups)) column.groups <- params$k_col

  new <- list(
			dendrogram = dendrogram
			, Rowv = row.reorder
			, Colv = column.reorder
			, distfun = distance.function
			, hclustfun = clustering.function
			, reorderfun = reorder.function
			, k_row = row.groups
			, k_col = column.groups
			, symm = symmetrical
	)

	params <- mergeLists(params, new)

	## call heatmap with the updated params and save
	## the params with the heatmap for later use
  ##==============================================
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
	d3heatmap$x$rows <- hm$rowDend
	d3heatmap$x$cols <- hm$colDend
	d3heatmap$x$matrix <- hm$mtx
  d3heatmap$x$image <- imgUri
	d3heatmap$x$params <- params

  return(d3heatmap)  
}

