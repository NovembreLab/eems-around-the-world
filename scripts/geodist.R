library(dplyr)
library(reshape2)
require(fields)
source("scripts/load_pop_meta.R")

geo_file <- snakemake@input[[1]]
outname <- snakemake@output$geodist
outname0 <- snakemake@output$geodist0


geo <- read.csv(geo_file)

ll <- c('longitude', 'latitude')
dmat <- rdist.earth(geo[,ll])
rownames(dmat) <- geo$popId
colnames(dmat) <- geo$popId
v <- melt(dmat)
names(v) <- c('popId.x', 'popId.y', 'geoDist')
v2 <- v %>% filter(popId.x <= popId.y)
write.csv(v2, outname0, row.names=F)

v <- v %>% filter(popId.x < popId.y)
write.csv(v, outname, row.names=F)
