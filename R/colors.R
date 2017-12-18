## The color construction routine. Previously these lines sat inside the 
## primary d3heatmap routine, but now have been broken out to allow for 
## calls from the new API

heatmapColors <- function(
  x
  , col
  , na.color
  , na.rm
  , rng
  , scale
  , breaks
  , symm
	
  ## need ability to accept extra args without using them
	, ...
) 
{

	## process colors and parameters to create the pallette
  if (is.factor(x)) {
    legend_colors <- scales::col_bin(col, domain = 1:length(factor(x)), 
                                     na.color = na.color)(1:length(factor(x)))
    col <- scales::col_factor(col, x, na.color = na.color)
    legend_bins <- sort(as.integer(factor(x)))
    legend_breaks <- legend_bins
    
  } else {
    if (is.null(rng)) {
          rng <- range(x, na.rm = TRUE)
          if (symm) rng <- c(max(abs(rng)), -max(abs(rng)))
    }
 
    ## create color-generating function(s) 
    vals <- unique(sort(x))
    if (is.null(breaks)) breaks <- 16
    
    if (length(breaks)==1) {
			# need to include all the possible breakpoints when getting the total 
			# color list for the legend
			breaks_ <- breaks
      vals <- seq(min(rng), max(rng), (max(rng) - min(rng)) / breaks_)
      
			cols <- scales::col_bin(col, rng, bins = vals, na.color = na.color)
      legend_colors <- unique(cols(vals))
      legend_breaks <- vals
      
    } else {
      breaks_ <- unique(sort(c(breaks, rng)))
      cols <- scales::col_bin(col, rng, bins = breaks_, na.color = na.color)
      
      # need to include both forced bins and the original values to get
      # the union of all possible bins (some bins might not have any instances)
      vals <- c(breaks_, vals)
      legend_colors <- unique(cols(vals))
      legend_breaks <- breaks_
    
    }
    legend_bins <- length(legend_colors)
  
  }

	# create the list to return to the calling environment
	lst <- list(
		legend_colors = legend_colors
		, legend_bins = legend_bins
		, legend_breaks = legend_breaks
		, col = cols
		, rng = rng
		, breaks = breaks_
	)	

	return(lst)
}
