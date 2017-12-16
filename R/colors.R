## The color construction routine. Previously these lines sat inside the 
## primary d3heatmap routine, but now have been broken out to allow for 
## calls from the new API

heatmapColors <- function(x, colors, na.color, na.rm, rng, scale, bins) {

	## process colors and parameters to create the pallette
  if (is.factor(x)) {
    legend_colors <- scales::col_bin(colors, domain = 1:length(factor(x)), 
                                     na.color = na.color)(1:length(factor(x)))
    colors <- scales::col_factor(colors, x, na.color = na.color)
    
  } else {
    if (is.null(rng)) {
          rng <- range(x, na.rm = TRUE)
          if (scale %in% c("row", "column")) {
              rng <- c(max(abs(rng)), -max(abs(rng)))
          }
    }
  
    if(is.null(bins)) {
      bins <- 50
      colors <- scales::col_numeric(colors, rng, na.color = na.color)
    } else {
      colors <- scales::col_bin(colors, rng, bins = bins, na.color = na.color)
    }
    legend_colors <- scales::col_bin(colors, 
																		 domain = 1:bins, 
																		 bins = bins, 
																		 na.color = na.color)(1:bins)
  }

	# create the list to return to the calling environment
	lst <- list(
		legend_colors
		, colors
		, rng
		, bins
	)	

	return(lst)
}
