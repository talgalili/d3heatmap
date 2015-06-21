# When serializing a dendrogram to a tree, we will limit ourselves
# to the following fields.
fields <- c("members", "height", "label")
nodeParFields <- c("pch", "cex", "col", "xpd", "bg")
edgeParFields <- c("col", "lty", "lwd")


# Serialize a dendrogram object to a d3-friendly tree. The main
# requirement is that nodes are lists with child nodes in a
# field named `children`.
dendToTree <- function(dend) {
  
  toList <- function(x) {
    if (is.null(x))
      NULL
    else
      as.list(x)
  }
  
  # Gather the fields for this node
  tree <- c(
    as.list(attributes(dend)[fields]),
    #list(order = {if(is.leaf(dend)) as.numeric(dend) else 0L}),
    list(nodePar = toList(attr(dend, "nodePar")[nodeParFields])),
    list(edgePar = toList(attr(dend, "edgePar")[edgeParFields]))
  )
  
  # In the future, this could either be fixed here, or in the javascript.
  if (is.null(tree$edgePar$col))
    tree$edgePar$col <- "" # Default to empty so CSS takes over (for dark theme)
  else
    tree$edgePar$col <- rgb(t(col2rgb(tree$edgePar$col, alpha = TRUE)), maxColorValue = 255)
  
  filter_null <- function(x) Filter(Negate(is.null), x)
  filter_NA <- function(x) Filter(Negate(is.na), x)
  # The next line is essential since without it we might get NULLs in the nodePar (etc.) when the tree is colored
  # and it would cause an error in plotting: 
      #   Error in if (length(nms) != n || any(nms == "")) stop("'options' must be a fully named list, or have no names (NULL)") : 
      #     missing value where TRUE/FALSE needed
  tree <- lapply(tree, filter_null)
  tree <- lapply(tree, filter_NA)
  

  # Recursively add children
  if (!is.leaf(dend)) {
    tree$children <- lapply(dend, dendToTree)
  }
  
  filter_null(tree)
}



is.dendrogram <- function (x) { inherits(x, "dendrogram")  }


# dendextend:::`labels<-.dendrogram`
`labels<-.dendrogram` <- function (object, ..., value) 
{
  if (missing(value)) {
      warning("value is missing, returning the dendrogram as is")
    return(object)
  }
  new_labels <- value
  new_labels_length <- length(new_labels)
  leaves_length <- length(order.dendrogram(object))
  
  if (new_labels_length < leaves_length) {
    warning("The lengths of the new labels is shorter than the number of leaves in the dendrogram - labels are recycled.")
    new_labels <- rep(new_labels, length.out = leaves_length)
  }
  .change.label.LTR <- function(dend_node) {
    if (is.leaf(dend_node)) {
      attr(dend_node, "label") <- new_labels[i_leaf_number]
      i_leaf_number <<- i_leaf_number + 1
    }
    return(unclass(dend_node))
  }
  i_leaf_number <- 1
  new_dend_object <- dendrapply(object, .change.label.LTR)
  class(new_dend_object) <- "dendrogram"
  return(new_dend_object)
}












if(FALSE) {
  
  
  x <- hclust(dist(1:3))
  library(dendextend)
  d <- color_branches(as.dendrogram(x), k = 2)
  plot(d)
  
  #source this whole file first!
  str(hclustToTree(x)[[1]])
  str(dendToTree(as.dendrogram(x)))
  str(dendToTree(d))
  
  str(dendToTree(row_dend2))
  
  
  dendToTree(dend)
  
}

