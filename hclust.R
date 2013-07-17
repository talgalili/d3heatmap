library(RJSONIO)

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

render <- function(data) {
  matrix <- as.matrix(data)
  
  rng <- range(matrix)
  
#   rowClust <- hclust(dist(matrix))
#   matrix <- matrix[rowClust$order,]
#   colClust <- hclust(dist(t(matrix)))
#   matrix <- matrix[,colClust$order]
  tmp <- tempfile()
  png(tmp)
  hm <- heatmap(data, keep.dendro=TRUE)
  dev.off()
  unlink(tmp)
  rowClust <- as.hclust(hm$Rowv)
  colClust <- as.hclust(hm$Colv)
  matrix <- matrix[hm$rowInd, hm$colInd]
  
  rowDend <- toJSON(hclustToTree(rowClust)[[1]], pretty=TRUE)
  colDend <- toJSON(hclustToTree(colClust)[[1]], pretty=TRUE)
  
  matrix <- toJSON(list(data = as.numeric(t(matrix)),
                        dim = dim(matrix),
                        rows = row.names(matrix),
                        cols = names(matrix)))
  
  domain <- toJSON(seq.int(rng[1], rng[2], length.out = 100))
  
  colors <- topo.colors(100)
  colors <- toJSON(sub('FF$', '', colors))
  #colors <- "['yellow', 'green']"
  
  html <- paste(readLines('template.html', warn=FALSE), collapse='\n')
  html <- sub('{{domain}}', domain, html, fixed = TRUE)
  html <- sub('{{colors}}', colors, html, fixed = TRUE)
  html <- sub('{{rowDend}}', rowDend, html, fixed = TRUE)
  html <- sub('{{colDend}}', colDend, html, fixed = TRUE)
  html <- sub('{{matrix}}', matrix, html, fixed = TRUE)
  file <- tempfile('hclust', fileext='.html')
  writeLines(html, file)
  return(file)
}
#browseURL(render(mtcars))
browseURL(render(exprs(esetSel)))
