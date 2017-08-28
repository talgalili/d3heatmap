### This is a fork of d3Heatmap for purposes of incorporating community PRs and issues logged in the main repo. 

### As the main repo is not currently being actively maintained, feel free to offer PRs and issues here. I actively use heatmaps in my projects and like this implementation over other non-d3 based heatmaps, so I will be active in maintaining this repo.

### Please note, my time is constrained (like most of us), so I will have limited time to implement major new features on my own; I appreciate all assistance from the community for processing issues and new features.

### In the future I hope to offer an updated version back to Rstudio for pulling into the main repo, and then hopefully an udpated CRAN submission.
___

### Checkout the Rebuild branch of this fork for current status of development. Developments there will soon be merged into the main branch
___
## D3 Heatmap for R

This is an R package that implements a heatmap [htmlwidget](http://htmlwidgets.org). It has the following features:

* Highlight rows/columns by clicking axis labels
* Click and drag over colormap to zoom in (click on colormap to zoom out)
* Optional clustering and dendrograms, courtesy of `base::heatmap`

### Example

http://rpubs.com/jcheng/mtcars-heatmap

### Installation

To install:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/d3heatmap")
```

### Usage

Like any htmlwidget, you can visualize a d3 heatmap directly from the R console:

```r
library(d3heatmap)
d3heatmap(mtcars, scale = "column", colors = "Spectral")
```

You can also include them in R Markdown chunks, or use them in Shiny applications with the `d3heatmapOutput` and `renderD3heatmap` functions.

See `?d3heatmap` for options.
