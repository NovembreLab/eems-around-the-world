library(dplyr)
library(reshape2)
require(fields)

ipmap <- read.table(snakemake@input$ipmap)
demes <- read.table(snakemake@input$demes)
sampled_demes <- sort(unique(ipmap[,1]))

outname <-  snakemake@output[[1]]

dmat <- rdist.earth(demes[sampled_demes,], miles=F)
rownames(dmat) <- sampled_demes
colnames(dmat) <- sampled_demes
v <- melt(dmat)
names(v) <- c('grid.x', 'grid.y', 'geoDist')
v <- v %>% filter(grid.x < grid.y)
write.csv(v, outname, row.names=F)

