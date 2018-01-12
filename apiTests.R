library(magrittr)

rsc<-matrix(rep_len(c('good', 'bad', 'ugly'), length.out = 64), ncol = 2)
rsccols<-c('red', 'white', 'blue')
rscnames <- c('Row 1', 'Row 2')

csc<-matrix(rep_len(c('first', 'second', 'third', 'fourth', 'fifth'), length.out = 33), nrow = 3)
csccols<-c('orange', 'blue', 'grey', 'green', 'red')
cscnames <- c('Column 1', 'Column 2', 'Column 3')

library(d3heatmap)
d3heatmap(mtcars,
          key = TRUE, scale = 'column', 
          key.title = "Legend", 
          col = 'RdYlGn',
          srtCol = 30, 
          breaks = 8,
          xlab = 'test',
          ylab = 'TEST',
          print.values = T,
          density.info = 'histogram',
          denscol = 'grey',
          sideCol = 3,
          sideRow = 4,
          # RowSideColors = rsc,
          ColSideColors = csc,
          # RowColorsPalette = rsccols,
          ColColorsPalette = csccols,
          # RowColorsNames = rscnames,
          ColColorsNames = cscnames) %>% 
  hmColors(color.bins = 16) %>% 
  hmLegend(density = 'histogram', density.fill = 'blue')

d3heatmap(mtcars, key = TRUE, scale = 'none') %>% 
  hmSideColors(axis = 'column', side.colors = csc,
    palette = csccols, names = cscnames) %>% 
  hmSideColors(axis = 'y', side.colors = rsc,
    palette = rsccols, names = rscnames)

rsc<-rep_len(c('blue', 'black', 'red'), length.out = 32)
csc<-rep_len(c('red', 'white', 'blue'), length.out = 11)

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
