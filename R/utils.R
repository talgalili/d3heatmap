# VERY useful function for combing parameter lists without tediousness
mergeLists <- function (base_list, overlay_list, recursive = TRUE) {
  if (length(base_list) == 0)
    overlay_list
  else if (length(overlay_list) == 0)
    base_list
  else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive)
        merged_list[[name]] <- mergeLists(base, overlay)
      else {
        merged_list[[name]] <- NULL
        merged_list <- append(merged_list,
                              overlay_list[which(names(overlay_list) %in% name)])
      }
    }
    merged_list
  }
}

# nice is-not-null-binary operator
`%||%` <- function(a, b) {
  if (!is.null(a)) return(a)
  b
}

# are_colors   
# Helper function to check for valid color strings  
are.colors <- function(x) {  
  if(is.numeric(x)) return(FALSE)  
  sapply(x, function(x) {  
    res <- try(col2rgb(x), silent=TRUE)  
    return(!"try-error" %in% class(res))  
  })  
}  

# some color functions include alphas, but alphas not supported in some
# browsers (display as black). Helper function to truncate the alphas
to.hex <- function(x) {
  sapply(x, function(x) {
    x <- as.vector(col2rgb(x))
    out <- substr(rgb(x[1], x[2], x[3], 
        alpha = 0, maxColorValue=255), 0, 7)
  })
}

