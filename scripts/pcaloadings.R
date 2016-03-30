require(ggplot2)
#! called by rule make_ladings_plots in pca.snake
load.test = F



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



plot.pc.loadings <- function(i, pcs, bim, cutoff=0.5){
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
    png(file=output, width=3200, height=1600)
    multiplot(plotlist=l, file=output, cols=4)
    dev.off()
}

args <- commandArgs(T)
if(length(args) >= 3){
    pcs <- read.table(args[1])
    bim <- read.table(args[2])
    output <- args[3]
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




