library(dplyr)
library(reshape2)
require(fields)
source("scripts/load_pop_meta.R")

pcs_file <- snakemake@input[[1]]
outname <- snakemake@output[[1]]
npc <- snakemake@wildcards$npc

pc_name <- sprintf('pcDist%s', npc) 

n_pcs <- snakemake@wildcards$npc

pc_medians <- read.csv(pcs_file)
rownames(pc_medians) <- pc_medians$popId
pc_medians %>% select(-popId) -> pc_medians

dmat <- as.matrix(dist(pc_medians[,1:n_pcs]))
v <- melt(dmat)
names(v) <- c('popId.x', 'popId.y', pc_name)
v <- v %>% filter(popId.x < popId.y)
write.csv(v, outname, row.names=F)

