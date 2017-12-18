library(magrittr)
library(d3heatmap)

d3heatmap(mtcars,
          key = TRUE, scale = 'row', 
          key.title = "Legend", 
          col = 'RdYlGn',
          srtCol = 30, 
          breaks = 8,
          xlab = 'test',
          ylab = 'TEST',
          print.values = T,
          density.info = 'none',
          denscol = 'grey',
          sideRow = 2,
          sideCol = 1,
          labRowSize = 150) %>% 
  hmColors(color.bins = 16) %>% 
  hmLegend(density = 'histogram', density.fill = 'blue')

# doesn't resize when defining axis titles
d3heatmap(mtcars, dendrogram = 'none', key = TRUE, col = 'RdYlGn',
          scale = 'column', key.title = "Legend", print.values = T,
          notecol = 'white') %>% 
  hmAxis("x", title = "test", location = 'bottom') %>% 
  hmAxis("y", title = "test", location = 'left') %>% 
  hmCells(font.size = 8, color = 'blue')

d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          key.title = "Legend", 
          key = T) %>%
  hmLegend(show = T, title = "Title", location = "tl")


d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          srtCol = 30) %>% 
  hmCells(digits = 0L, print = T)

d3heatmap(mtcars, 
          scale = 'column') %>% 
  hmDendrogram(dendrogram = 'none') %>% 
  hmDendrogram(dendrogram = 'row', groups = 2) %>% 
  hmDendrogram(dendrogram = 'col', groups = 2)

d3heatmap(mtcars, 
          scale = 'row',
          dendrogram = 'none',
          srtCol = 30,
          col = "Blues",
          breaks = 5,
          symbreaks = T,
          key = T,
          key.location = 'bl') %>% 
  hmColors(colors = 'RdYlGn', color.bins = 12, symmetrical = F, theme = 'dark')

d3heatmap(mtcars, 
          dendrogram = 'none', 
          scale = 'column', 
          srtCol = 30) %>% 
  hmColors(colors = 'RdYlGn', color.bins = 12, symmetrical = T)
  hmDendrogram(dendrogram = 'row', groups = 3) %>% 
  hmDendrogram(dendrogram = 'col', groups = 4) %>% 
  hmCells(digits = 0L, print = T) %>% 
  hmLegend(show = T, title = "Title", location = "bl") %>% 
  hmAxis("x", angle = 30, title = "test", location = 'top', font.size = '24px') %>% 
  hmAxis("y", title = "test", location = 'right')