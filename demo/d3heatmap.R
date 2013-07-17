library(heatmap)

source("http://www.bioconductor.org/biocLite.R")
if (!require(ALL))
  biocLite("ALL")
if (!require(limma))
  biocLite("limma")

library(ALL)
data("ALL")
eset <- ALL[, ALL$mol.biol %in% c("BCR/ABL", "ALL1/AF4")]
#eset <- ALL
#heatmap(exprs(eset[1:100,]))

library(limma)
f <- factor(as.character(eset$mol.biol))
design <- model.matrix(~f)
fit <- eBayes(lmFit(eset,design))

selected  <- p.adjust(fit$p.value[, 2]) <0.015
esetSel <- eset [selected, ]

heatmap(exprs(esetSel), col=topo.colors(100))

library(shiny)

showHeatmap(exprs(esetSel))
# runApp(
#   list(
#     ui = basicPage(
#       heatmapOutput('heatmap')
#     ),
#     server = function(input, output) {
#       output$heatmap <- renderHeatmap(
#         exprs(esetSel)
#       )
#     }
#   )
# )
