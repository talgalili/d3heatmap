**d3heatmap is not actively developed, but I will accept PR. You might consider using  [heatmaply](https://github.com/talgalili/heatmaply), which is based on plotly (it comes with more features, but is not based on d3)**


# D3 Heatmap for R

This is an R package that implements a heatmap [htmlwidget](http://htmlwidgets.org). It has the following features:

* Highlight rows/columns by clicking axis labels
* Click and drag over colormap to zoom in (click on colormap to zoom out)
* Optional clustering and dendrograms, courtesy of `base::heatmap`

### Example

### Installation

To install:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("talgalili/d3heatmap")
```

### Usage

Like any htmlwidget, you can visualize a d3 heatmap directly from the R console:

```r
library(d3heatmap)
d3heatmap(mtcars, scale = "column", colors = "Spectral")
```
You can also include them in R Markdown chunks, or use them in Shiny applications with the `d3heatmapOutput` and `renderD3heatmap` functions.

See `?d3heatmap` for options.

### Modern API

Inspired by _dygraphs_ and able to leverage _magrittr_, the new API provides a second method for invoking d3heatmap and integrating with Shiny apps, modules, and gadgets! Examples:

```r
library(d3heatmap)
library(magrittr)

d3heatmap(mtcars, dendrogram = 'none', key = TRUE, col = 'RdYlGn',
          scale = 'column', key.title = "Legend", print.values = T,
          notecol = 'white') %>% 
  hmAxis("x", title = "test", location = 'bottom') %>% 
  hmAxis("y", title = "test", location = 'left') %>% 
  hmCells(font.size = 8, color = 'blue') %>% 
  hmLegend(show = T, title = "Title", location = "tl")
  
```
### Side colors

```r
library(d3heatmap)
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
          RowSideColors = rsc,
          ColSideColors = csc,
          RowColorsPalette = rsccols,
          ColColorsPalette = csccols,
          RowColorsNames = rscnames,
          ColColorsNames = cscnames)

## using the modern API:

d3heatmap(mtcars, key = TRUE, scale = 'none') %>% 
  hmSideColors(axis = 'column', side.colors = csc,
    palette = csccols, names = cscnames) %>% 
  hmSideColors(axis = 'y', side.colors = rsc,
    palette = rsccols, names = rscnames)
    
```

# NEWS from recent PR



### __2018-02-24 Gadget


An shiny gadget coupled with an `S4` class that provides `print()` and 
`save()` methods. Gadget takes normal `d3heatmap` inputs and allows for 
interactive adjustment of the heatmap. Gadget allows for filtering rows 
and columns, and also a dynamic filter to interatively subset the entire 
underlying data set. Saving the gadget to an object generates the `S4` class 
that contains the heatmap, data, filter, and settings.  Passing the gadget 
back into the function `d3heatmapGadget(gadget)` starts the user at the last 
state of the gadget.

```r
gadget <- d3heatmapGadget(mtcars, col = 'blues')
print(gadget)
save(gadget, file = "heatmap.html")
gadget <- d3heatmapGadget(gadget)
    
```


### __2018-01-13__ Side colors

Based on example contributions and forks from several people, the side colors components of heatmap.2 and heatmap.3 have been added! Functionality includes color labels, axis labels for the color sections, and hover info. Further alignment of the old API parameters to heatmap.2 and heatmap.3, plus heatmap.2/3 and the modern API were implemented for side colors. the Readme, package news and examples were also updated.

### __2017-12-17__ New API!!  

The master branch now includes a newer, modern API, motivated by the main d3heatmap fork's desire for a new API and inspired by the API of the _dygraphs_ package produced by _RStudio_.  The new API takes advantage of _magrittr_ piping and offers smaller functions to modify selected portions of the heatmap. I have conducted good, but by no means exhaustive, testing... so feel free to poke around, find bugs, and open up issues or PRs for them.
