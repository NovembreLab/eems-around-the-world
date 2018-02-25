library(dplyr)
library(reshape2)
require(fields)
require(readr)
require(abind)
source("scripts/load_pop_meta.R")


statname <- snakemake@params$statname
if(is.null(statname))statname <- 'eemsdist'

Dhat <- snakemake@input$Dhat
order <- snakemake@input$order
indiv_meta_file <- snakemake@input$indiv_meta
ipmap_file <- snakemake@input$ipmap

outname <- snakemake@output[[1]]
outname2 <- snakemake@output[[2]]
outname3 <- snakemake@output[[3]]


#load mats
l <- lapply(Dhat, read.table)    
l[['along']] = 3                 
a <- do.call(abind, l) 
dmat <- apply(a, 1:2, mean)


inds <- read_delim(order, col_names=F, delim=" ")[,1]
ipmap <- read.table(ipmap_file)
inds <- cbind(inds, ipmap)
names(inds) <- c('sampleId', 'grid')
indiv_meta <- read_csv(indiv_meta_file)
inds %>% left_join(indiv_meta) -> inds
pops <- inds %>% group_by(popId) %>% 
    summarize(grid=first(grid), n=n())
write.csv(pops, outname2, row.names=F)

griddict <- pops$grid
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
names(v) <- c('popId.x', 'popId.y', statname)

v2 <- v %>% filter(popId.x <= popId.y)
write.csv(v2, outname3, row.names=F)

v <- v %>% filter(popId.x < popId.y)
write.csv(v, outname, row.names=F)


save.image(".rsnakemakedebug")


