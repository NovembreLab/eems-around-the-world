library(dplyr)
library(reshape2)
require(fields)
require(readr)
require(abind)
source("scripts/load_pop_meta.R")



Dhat <- snakemake@input$Dhat
order <- snakemake@input$order
indiv_meta_file <- snakemake@input$indiv_meta
ipmap_file <- snakemake@input$ipmap

outname <- snakemake@output[[1]]


#load mats
l <- lapply(Dhat, read.table)    
l[['along']] = 3                 
a <- do.call(abind, l) 
dmat <- apply(a, 1:2, mean)


inds <- read_delim(order, col_names=F, delim=" ")[,1]
ipmap <- read.table(ipmap_file)
inds <- cbind(inds, ipmap)
names(inds) <- c('sampleId', 'gridId')
indiv_meta <- read_csv(indiv_meta_file)
inds %>% left_join(indiv_meta) -> inds
pops <- inds %>% group_by(popId) %>% summarize(gridId=first(gridId))

griddict <- pops$gridId
names(griddict) <- pops$popId

unique_pops <- unique(inds$popId)
n_pops <- length(unique_pops)

res <- matrix(NA, nrow=n_pops, ncol=n_pops)
for(i in 1:n_pops){for(j in 1:n_pops){
    x <- griddict[as.character(unique_pops[i])]
    y <- griddict[as.character(unique_pops[j])]
    res[i,j] <- dmat[x, y]
}}

rownames(res) <- unique_pops
colnames(res) <- unique_pops   
v <- melt(res)
names(v) <- c('popId.x', 'popId.y', 'eemsdist')
v <- v %>% filter(popId.x < popId.y)
write.csv(v, outname, row.names=F)

save.image(".rsnakemakedebug")


