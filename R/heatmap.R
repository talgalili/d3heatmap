# This file contain functions for creating a list-of-lists representation of trees
# so that they could be used by json later to create the needed tree in the heatmap



createLeafNode <- function(hclust, i) {
  list(name = hclust$labels[[i]],
       order = hclust$order[[i]])
}

hclustToTree <- function(hclust) {
  if (length(hclust$merge) == 0)
    return(NULL)
  
  merges <- list()
  for (index in 1:nrow(hclust$merge)) {
    left <- hclust$merge[index, 1]
    right <- hclust$merge[index, 2]
    
    if (left < 0)
      left <- createLeafNode(hclust, -left)
    else
      left <- merges[[left]]
    if (right < 0)
      right <- createLeafNode(hclust, -right)
    else
      right <- merges[[right]]
    
    if (left$order > right$order) {
      tmp <- left
      left <- right
      right <- tmp
    }
    
    merges[[index]] <- list(
      children = list(
        left,
        right
      ),
      order = left$order
    )
  }
  
  return(merges[nrow(hclust$merge)])
}





dendToTree <- function(dend) {
  
  node_attr_to_list <- function(node) {
    list(
      members = attr(node, "members"),
      height = attr(node, "height"),
      # name = attr(node, "label"),  # I would have rathered this be called label!
      label = attr(node, "label"),
      leaf = attr(node, "leaf"),
      order = {if(is.leaf(node)) as.numeric(node) else 0L} ,

      nodePar_pch = attr(node, "nodePar")$pch,
      nodePar_cex = attr(node, "nodePar")$cex,
      nodePar_col = attr(node, "nodePar")$col,
      nodePar_xpd = attr(node, "nodePar")$xpd,
      nodePar_bg = attr(node, "nodePar")$bg,
            
      edgePar_col = attr(node, "edgePar")$col,
      edgePar_lty = attr(node, "edgePar")$lty,
      edgePar_lwd = attr(node, "edgePar")$lwd
#       edgePar_p.col = attr(node, "edgePar")$p.col,
#       edgePar_p.lty = attr(node, "edgePar")$p.lty,
#       edgePar_p.lwd = attr(node, "edgePar")$p.lwd,
      
    )
  }
  
  grow_tree <- function(dend) {
    if(is.leaf(dend)) {
      return(list(leaf = list(attr = node_attr_to_list(dend))))
    } #else
    
    children <- list()
    for(i in 1:length(dend)) {
      node <- dend[[i]]
      
      if(is.leaf(node)) {
        children <- c(children, leaf = list(list(attr = node_attr_to_list(node))) ) 
      } else {
        children <- c(children, node = list(grow_tree(node))) 
      }
    }
   
    list(children = children, attr = node_attr_to_list(dend))  
  }

  grow_tree(dend)
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

