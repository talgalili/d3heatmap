#### This is a fork of d3Heatmap for purposes of incorporating community PRs and issues logged in the main repo. 

#### As the main repo is not currently being actively maintained, feel free to offer PRs and issues here. I actively use heatmaps in my projects and like this implementation over other non-d3 based heatmaps, so I will be active in maintaining this repo.

#### Please note, my time is constrained (like most of us), so I will have limited time to implement major new features on my own; I appreciate all assistance from the community for processing issues and new features.

#### In the future I hope to offer an updated version back to Rstudio for pulling into the main repo, and then hopefully an udpated CRAN submission.
___

### __2017-12-17__ New API!!  

The master branch now includes a newer, modern API, motivated by the main d3heatmap fork's desire for a new API and inspired by the API of the _dygraphs_ package produced by _RStudio_.  The new API takes advantage of _magrittr_ piping and offers smaller functions to modify selected portions of the heatmap. I have conducted good, but by no means exhaustive, testing... so feel free to poke around, find bugs, and open up issues or PRs for them.

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
