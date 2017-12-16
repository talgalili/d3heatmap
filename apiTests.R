library(dplyr)

d3heatmap(mtcars, dendrogram = 'none', 
          show.legend = TRUE, scale = 'column', 
          legend.title = "Legend", xaxis_angle = 30, xaxis_title = 'test')

# doesn't resize when defining axis titles
d3heatmap(mtcars, dendrogram = 'none', show.legend = TRUE, 
          scale = 'column', legend.title = "Legend") %>% 
  hmAxis("x", angle = 30, title = "test")

d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          legend.title = "Legend", 
          xaxis_angle = 30, 
          xaxis_title = 'test') %>%
  hmLegend(show = TRUE, title = "Title", location = "tr")

debug(hmCells)
d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          legend.title = "Legend", 
          xaxis_angle = 30, 
          xaxis_title = 'test') %>%
  hmCells(digits = 2L, print = T) %>% 
  hmLegend(show = F)