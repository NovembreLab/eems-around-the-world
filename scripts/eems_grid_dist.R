library(dplyr)
library(reshape2)
require(fields)
require(readr)
require(abind)



D <- snakemake@input$mat

outname <- snakemake@output[[1]]


#load mats
l <- lapply(D, read.table)    
l[['along']] = 3                 
a <- do.call(abind, l) 
dmat <- apply(a, 1:2, mean)
rownames(dmat) <- 1:nrow(dmat)
colnames(dmat) <- 1:nrow(dmat)
v <- melt(dmat)
names(v) <- c('popId.x', 'popId.y', 'eemsdist')
v <- v %>% filter(popId.x < popId.y)
write.csv(v, outname, row.names=F)




