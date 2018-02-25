library(dplyr)
library(reshape2)
require(fields)
source("scripts/load_pop_meta.R")

pc <- snakemake@input[['pc']]
fam <- snakemake@input[['fam']]
indiv_meta <- snakemake@input[['indiv_meta']]
pop_display <- snakemake@input$pop_display
outname <- snakemake@output[['pcdist']]
npc <- snakemake@wildcards$npc
PCS <- sprintf("PC%s", 1:npc)
pc_name <- sprintf("pcDist%s", npc)

data <- load_pca_data(pc, fam, indiv_meta, pop_display)
rownames(data) <- data$sampleId
dists <- as.matrix(dist(data[,PCS]))

v <- melt(dists)
names(v) <- c('sampleId.x', 'sampleId.y', pc_name)
v$sampleId.x <- as.character(v$sampleId.x)
v$sampleId.y <- as.character(v$sampleId.y)
v <- v %>% filter(sampleId.x < sampleId.y)
write.csv(v, outname, row.names=F)

