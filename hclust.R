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
  json <- RJSONIO::toJSON(hclustToTree(hclust(dist(data)))[[1]], pretty=T)
  template <- paste(readLines('template.html', warn=FALSE), collapse='\n')
  html <- sub('{{data}}', json, template, fixed = TRUE)
  file <- tempfile('hclust', fileext='.html')
  writeLines(html, file)
  return(file)
}
browseURL(render(mtcars))
