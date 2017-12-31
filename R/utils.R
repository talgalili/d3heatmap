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

`%||%` <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

#' are_colors   
#' Helper function to check for valid color strings  
#' @param x color vector (etc) to test.   
are.colors <- function(x) {  
  if(is.numeric(x)) return(FALSE)  
  sapply(x, function(X) {  
    res <- try(col2rgb(x), silent=TRUE)  
    return(!"try-error" %in% class(res))  
  })  
}  
