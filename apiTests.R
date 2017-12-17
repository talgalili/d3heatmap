library(dplyr)

d3heatmap(mtcars, dendrogram = 'none', 
          show.legend = TRUE, scale = 'column', 
          legend.title = "Legend", xaxis_angle = 30, xaxis_title = 'test')

# doesn't resize when defining axis titles
debug(hmAxis)
d3heatmap(mtcars, dendrogram = 'none', show.legend = TRUE, colors = 'RdYlGn',
          scale = 'column', legend.title = "Legend") %>% 
  hmAxis("x", angle = 30, title = "test", location = 'top', font.size = '24px') %>% 
  hmAxis("y", title = "test", location = 'right')

d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          legend.title = "Legend", 
          xaxis_angle = 30, 
          xaxis_title = 'test',
          show.legend = T) %>%
  hmLegend(show = T, title = "Title", location = "bl")

debug(hmCells)
d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          xaxis_angle = 30) %>% 
  hmCells(digits = 0L, print = T)

debug(hmDendrogram)
d3heatmap(mtcars, 
          scale = 'column', 
          xaxis_angle = 30) %>% 
  hmDendrogram(dendrogram = 'none') %>% 
  hmDendrogram(dendrogram = 'row', groups = 2) %>% 
  hmDendrogram(dendrogram = 'col', groups = 2)

debug(hmColors)
d3heatmap(mtcars, 
          scale = 'column', 
          xaxis_angle = 30) %>% 
  hmColors(theme = 'dark', colors = 'RdYlGn', color.bins = 4, symmetrical = T)

d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          xaxis_angle = 30) %>% 
  hmDendrogram(dendrogram = 'row', groups = 3) %>% 
  hmDendrogram(dendrogram = 'col', groups = 4) %>% 
  hmCells(digits = 0L, print = T) %>% 
  hmLegend(show = T, title = "Title", location = "bl") %>% 
  hmAxis("x", angle = 30, title = "test", location = 'top', font.size = '24px') %>% 
  hmAxis("y", title = "test", location = 'right')