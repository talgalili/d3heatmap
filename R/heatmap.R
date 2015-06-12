# When serializing a dendrogram to a tree, we will limit ourselves
# to the following fields.
fields <- c("members", "height", "label")
nodeParFields <- c("pch", "cex", "col", "xpd", "bg")
edgeParFields <- c("col", "lty", "lwd")


# Serialize a dendrogram object to a d3-friendly tree. The main
# requirement is that nodes are lists with child nodes in a
# field named `children`.
dendToTree <- function(dend) {
  
  # Gather the fields for this node
  tree <- c(
    as.list(attributes(dend)[fields]),
    #list(order = {if(is.leaf(dend)) as.numeric(dend) else 0L}),
    list(nodePar = attr(dend, "nodePar")[nodeParFields]),
    list(edgePar = attr(dend, "edgePar")[edgeParFields])
  )

  # Recursively add children
  if (!is.leaf(dend)) {
    tree$children <- lapply(dend, dendToTree)
  }
  
  Filter(Negate(is.null), tree)
}

if(FALSE) {
  
  
  x <- hclust(dist(1:3))
  library(dendextend)
  d <- color_branches(as.dendrogram(x), k = 2)
  plot(d)
  
  #source this whole file first!
  str(hclustToTree(x)[[1]])
  str(dendToTree(d))
  
}

