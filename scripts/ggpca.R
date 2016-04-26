library(ggplot2)
library(data.table)
#! called from snakefiles/pca.snake:make_pc_plots

makePC <- function(data, n, col, field='abbrev'){
    f = sprintf('factor(%s)' , field)
    id <- sprintf('PC%d', n)
    g <- ggplot(data, aes_string(f, id, fill=field))
    g <- g + geom_violin(adjust=.2) + col
    g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              legend.position="none"
              )
    g
}

make2PC <- function(data, i, j, col){
    id1 <- sprintf('PC%d', i)
    id2 <- sprintf('PC%d', j)
    
    data2 <- data[sample.int(nrow(data), nrow(data)),]
    g <- ggplot(data2,aes_string(id1, id2, colour='abbrev', label='abbrev')) +
        geom_text() + col
    g <- g + guides(colour=guide_legend(override.aes=list(alpha=1)))
    g + theme(legend.position='none')
}


means <- function(data){
    means <- aggregate(data[,-1], list(data$POP), mean)
}

makePlots <- function(data, col, output1, output2){
    nmax <- sum(substr(names(data),1,2) == 'PC') 
    p1 <- lapply(1:nmax, function(i) makePC(data, i, col))
    p2 <- lapply(seq(2,nmax, 2), function(i) make2PC(data, i-1, i, col))
    l = list(PC1=p1, PC2=p2)
    png(file=output1, width=3200, height=1600)
    multiplot(plotlist=p1, file=output, cols=4)
    dev.off()
    png(file=output2, width=3200, height=1600)
    multiplot(plotlist=p2, file=output, cols=5)
    dev.off()
}

load.data <- function(pc, fam,
                      indiv_meta, pop_display){
    indiv_meta <- read.csv(indiv_meta)
    pop_display <- read.csv(pop_display)
    indiv <- merge(indiv_meta, pop_display, all.x=T)

    fam <- read.table(fam)[,1]

    data <- data.frame(fread(pc))
    names(data) <- paste0("PC", 1:ncol(data))

    data <- cbind(fam, data, n=1:length(fam)) 
    names(data)[1] <- 'sampleId'
    m <- merge(indiv, data, all.y=T)
    m <- m[order(m$n),]

    return(m)
}

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
args <- commandArgs(T)
if(exists('snakemake')){
    pc <- snakemake@input[['pc']]
    fam <- snakemake@input[['fam']]
    indiv_meta <- snakemake@input[['indiv_meta']]
    pop_display <- snakemake@input[['pop_display']]
    output <- snakemake@output[['pc1']]
    output2 <- snakemake@output[['pc2']]
    data <- load.data(pc, fam, indiv_meta, pop_display)
    col <- list(scale_color_manual(name=data$abbrev, values=data$color),
                scale_fill_manual(name=data$abbrev, values=data$color))
    makePlots(data, col, output, output2)
} else{
    args <- commandArgs(T)
    pc <- args[1]
    fam <- args[2]
    indiv_meta <- args[3]
    pop_display <- args[4]
    output <- args[5]
    output2 <- args[6]
    data <- load.data(pc, fam, indiv_meta, pop_display)
    col <- list(scale_color_manual(name=data$abbrev, values=data$color),
                scale_fill_manual(name=data$abbrev, values=data$color))
    makePlots(data, col, output, output2)
}


#col <- col[names(col) %in% w$eusplit]    




#g1 <- makePC(pc)
#ggsave(opt, g1)






