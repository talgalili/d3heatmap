# D3 Heatmap for R

This is an R package that implements a heatmap [htmlwidget](http://htmlwidgets.org). It has the following features:

* Highlight rows/columns by clicking axis labels
* Click and drag over colormap to zoom in (click on colormap to zoom out)
* Optional clustering and dendrograms, courtesy of `base::heatmap`

<div style="color: red">**Critical:** If you are using a version of d3heatmap older than 0.4.0, please upgrade now! Previous versions put row and column names in the incorrect order!</div>

### Examples

http://rpubs.com/jcheng/d3heatmap  
http://rpubs.com/jcheng/d3heatmap_large

### Installation

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github('hadley/scales')
devtools::install_github("rstudio/d3heatmap")
```

### Usage

Like any htmlwidget, you can visualize a d3 heatmap directly from the R console:

```r
library(d3heatmap)
d3heatmap(scale(mtcars), colors = "Greens", theme = "dark")
```

You can also include them in R Markdown chunks, or use them in Shiny applications with the `d3heatmapOutput` and `renderD3heatmap` functions.

See `?d3heatmap` for options.
