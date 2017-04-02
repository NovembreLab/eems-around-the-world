library(dplyr)
library(reshape2)
require(fields)
require(readr)
source("scripts/load_pop_meta.R")

get_mean_indexed_dist <- function(x, y, dmat, popId, f=mean){
    subset.x <- which(popId == x)
    subset.y <- which(popId == y)
    f(dmat[subset.x, subset.y])
}


diffs <- snakemake@input$diffs
order <- snakemake@input$order
indiv_meta_file <- snakemake@input$indiv_meta

outname <- snakemake@output[[1]]

dmat <- read_delim(diffs, col_names=F, delim=" ")
dmat <- as.matrix(dmat[,-1])
inds <- read_delim(order, col_names=F, delim=" ")[,1]
names(inds) <- 'sampleId'
indiv_meta <- read_csv(indiv_meta_file)
inds %>% left_join(indiv_meta) -> inds

unique_pops <- unique(inds$popId)
n_pops <- length(unique_pops)
res <- matrix(NA, nrow=n_pops, ncol=n_pops)
for(i in 1:n_pops){for(j in 1:n_pops){
    x <- unique_pops[i]; y <- unique_pops[j]
    res[i,j] <- get_mean_indexed_dist(x, y, dmat, inds$popId)
}}

rownames(res) <- unique_pops
colnames(res) <- unique_pops   
v <- melt(res)
names(v) <- c('popId.x', 'popId.y', 'gendist')
v <- v %>% filter(popId.x < popId.y)
write.csv(v, outname, row.names=F)



