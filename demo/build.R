library(dplyr)
library(tidyr)
library(heatmap)

# Read CSV data
df <- read.csv("~/Downloads/summary99-0415.csv")
head(df)

# Drop unneeded columns, spread long to wide
spr <- df %>% select(-oddsRatio, -X) %>% spread(disease, logOR)
head(spr)
# Assign pretty row names
row.names(spr) <- spr$sample
# Drop the sample column, now that we're done with it
spr <- spr %>% select(-sample)
sprM <- as.matrix(spr)
sprM <- sprM[1:10,1:30]

colors <- rev(c("#800C38", "#BD122B", "#E31E16", "#FC3518", "#FD4919", "#FD7832",  "#FD9638", "#FE864C", "#FECB62", "#FFE98C"))
d3heatmap(sprM, colors = colors, theme = "dark")
#d3heatmap(matrix(runif(100), 10, 10), theme = 'dark')
