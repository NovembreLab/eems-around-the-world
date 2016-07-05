suppressPackageStartupMessages({
require(ggplot2)
})
#! called by rule make_ladings_plots in pca.snake
load.test = F


plot.pc.loadings <- function(i, pcs, bim, cutoff=0.9){
    pc <- sprintf("PC%s", i)
    pcs <- abs(pcs)
    names(bim) <- c('chr', 'rsid', 'NA', 'pos', 'a1', 'a2')
    names(pcs) <- sprintf("PC%d", 1:ncol(pcs))

    data <- data.frame(bim, pcs)
    q1 <- quantile(data[,pc], cutoff)
    d <- data[data[pc] > q1,]
    g <-  ggplot(d, aes_string(x='pos', y=pc,
                               colour='as.factor(chr)')) + 
            geom_point() + 
            facet_grid(.~ chr, scales='free', space='free_x') +
            scale_colour_manual(values=rep(c('black', 'grey'),21)) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  legend.position='none'
                  )
    g
}


make.plots <- function(pcs, bim, output){
    print(dim(pcs))
    l <- lapply(1:ncol(pcs), plot.pc.loadings, pcs, bim)
    for(i in 1:length(l)){
	ggsave(output[i], l[[i]], width=7, height=4)
    }
}


args <- commandArgs(T)
if(length(args) >= 3){
    pcs <- read.table(args[1])
    bim <- read.table(args[2])
    output <- args[3:length(args)]
    print(args)
    make.plots(pcs, bim ,output)
} else if(exists('snakemake')){ 
    load <- read.table(snakemake@input[['load']])
    bim <- read.table(snakemake@input[['bim']])
    output <- snakemake@output[['fig']]
    make.plots(load, bim, output)
} else if(load.test){ 
    pcs <- read.table("pca/flash_pruned_dim30.load") 
    bim <- read.table("data/hrc_merge5-pruned.bim")
    output <- 'test.rds'
}

