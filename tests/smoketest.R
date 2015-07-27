test_root <- "testresults"

serialize <- function(widget) {
  htmlwidgets:::toJSON2(widget, pretty=TRUE, digits = 12)
}

mock_d3heatmap_record <- function(...) {
  cat(format(sys.call(0)), "\n")
  d <- d3heatmap::d3heatmap(...)
  json <- serialize(d)
  cat(json, "\n")
}

with(list(d3heatmap = mock_d3heatmap_record), {
  
  set.seed(1001)
  
  x <- mtcars[c(2:4,7),1:4]
  
  # Test colors when dendextend isn't loaded
  d3heatmap(x, k_row = 3, k_col = 2)

  suppressPackageStartupMessages(library(dendextend))
  row_dend2 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 3)
  col_dend2 <- x %>% t %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 2)
  library(d3heatmap)
  d3heatmap(x, Rowv = row_dend2, Colv = col_dend2) # Works!
  d3heatmap(x)
  
  d3heatmap(x, Rowv = FALSE)
  d3heatmap(x, dendrogram = "no", labRow = 1:4)
  d3heatmap(x, dendrogram = "no", labCol = 1:4)
  d3heatmap(x, labRow = 1:4)
  
  
  row_dend2 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 3) %>% 
    set("branches_lwd", c(4,1)) %>%    
    set("branches_lty", c(1,1,3))
  plot(row_dend2)
  # for now, d3heatmap still ignores line type and width:
  d3heatmap(x) # Works!
  d3heatmap(x, digits = 0) # Works!
  d3heatmap(x, Colv = col_dend2) # Works!
  d3heatmap(x, Rowv = row_dend2, Colv = col_dend2) # Works!
  # str(unclass(row_dend2))
  
  
  d3heatmap(matrix(rnorm(10), 2,5), digits = 12) # Works!
  d3heatmap(matrix(rnorm(10), 2,5), digits = 2) # Works!
  
  
  # various examples  
  library(d3heatmap)
  d3heatmap(scale(mtcars), colors = "Greens", theme = "dark")
  d3heatmap(scale(mtcars), dendrogram = "none")
  
  d3heatmap(scale(mtcars))
  d3heatmap(scale(mtcars), dendrogram = "none")
  d3heatmap(scale(mtcars), Colv = FALSE)
  d3heatmap(scale(mtcars), Rowv = FALSE)
  d3heatmap(scale(mtcars), dendrogram = "row")
  d3heatmap(scale(mtcars), dendrogram = "col")
  
  
  
  x <- mtcars[c(2:4,7),1:4]
  d3heatmap(x)
  d3heatmap(x, dendrogram = "none")
  
  d3heatmap(x, scale = "row")
  scale(x)
  d3heatmap(scale(x), dendrogram = "none")
  d3heatmap(x, scale = "column", dendrogram = "none")
  d3heatmap(x, scale = "row", dendrogram = "none")
  
  d3heatmap(x, labRow = 1:4)
  d3heatmap(x, labCol = 1:4)
  
  heatmap(scale(mtcars[1:4,1:4]), Rowv = NA, Colv = NA)
  
  
  library(dendextend)
  d3heatmap(x)
  # gives the same results - yay:
  row_dend <- x %>% dist %>% hclust %>% as.dendrogram # %>% plot
  d3heatmap(x, Rowv = row_dend)
  row_dend2 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    color_branches(k = 2)
  plot(row_dend2)
  d3heatmap:::dendToTree(row_dend2[[2]][[2]])
  d3heatmap(x, Rowv = row_dend2) # Works!
  
  
  row_dend <- x %>% dist %>% hclust %>% as.dendrogram # %>% plot
  d3heatmap(x, Rowv = row_dend)
  
  
  # Next step!
  row_dend3 <- x %>% dist %>% hclust %>% as.dendrogram %>%
    set("branches_lwd", c(4,1)) %>%    
    set("branches_lty", c(1,1,3)) %>%  
    set("branches_col", c("gold","grey","blue", "red")) 
  row_dend3 <- set(row_dend3, "branches_col", c(1,2,3)) # TODO: this doesn't work - needs to be fixed...
  # plot(row_dend3) # line width and line type are still ignored.
  d3heatmap(x, Rowv = row_dend3) 
  labels(row_dend3) <- 1:4
  d3heatmap(x, Rowv = row_dend3) 
  
  d3heatmap(x, Rowv = row_dend3, xaxis_font_size = 30) 
  d3heatmap(x, Rowv = row_dend3, cexRow = 3) 
  
  
  
  d3heatmap(cor(iris[,2:3]), revC = TRUE)
  d3heatmap(cor(iris[,2:3]))
  heatmap(cor(iris[,2:3]))
  
  
})
