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

#' @export
heatmapOutput <- function(outputId) {
  shiny::addResourcePath('heatmap', system.file('www', package='heatmap'))
  tagList(
    singleton(
      tags$head(
        tags$link(rel='stylesheet', type='text/css', href='heatmap/heatmap.css'),
        tags$script(src='heatmap/heatmap.js'),
        tags$script(src='heatmap/binding.js'),
        HTML('<script src="http://d3js.org/d3.v3.js"></script>
<script src="http://code.jquery.com/jquery-1.10.2.min.js"></script>
<link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet">
<script src="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"></script>
')
      )
    ),
    tags$div(id=outputId, class='d3-heatmap')
  )
}

#' @export
renderHeatmap <- function(expr, env = parent.frame(1), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  
  function() {
    data <- func()
    
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
    
    rowDend <- hclustToTree(rowClust)[[1]]
    colDend <- hclustToTree(colClust)[[1]]

    
    domain <- seq.int(rng[1], rng[2], length.out = 100)
    
    colors <- topo.colors(100)
    colors <- sub('FF$', '', colors)
    
    matrix <- list(data = as.numeric(t(matrix)),
                   dim = dim(matrix),
                   rows = row.names(matrix),
                   cols = names(matrix),
                   colors = colors,
                   domain = domain)
    
    return(list(rows = rowDend, cols = colDend, matrix = matrix))
  }
}

#' @export
showHeatmap <- function(matrix) {
  shiny::runApp(
    list(
      ui = basicPage(
        heatmapOutput('heatmap')
      ),
      server = function(input, output) {
        output$heatmap <- renderHeatmap(
          matrix
        )
      }
    )
  )
}
#browseURL(render(mtcars))
#browseURL(render(exprs(esetSel)))
