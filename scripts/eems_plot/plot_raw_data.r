require(rworldmap)
source("scripts/eems_plot/contour.r")


plot_pca_from_input <- function(datapath){
	diff <- as.matrix(read.table(sprintf("%s.diffs", datapath)))
	meta <<- read.table(sprintf("%s.meta", datapath), header=T)
    o <<- as.matrix(read.table(sprintf("%s.order", datapath)))
	plot_pca1(diff, meta)
}

plot_pca1 <- function(diff, meta){
    p <<- prcomp(diff)
    pch=15+as.numeric(as.factor(meta$DATASET))
    par(mfrow=c(1,4))
    plot(p$x[,c(1,2)], col=get_col(meta), pch=get_pch(meta))
    plot(p$x[,c(1,3)], col=get_col(meta), pch=get_pch(meta))
    plot(p$x[,c(1,4)], col=get_col(meta), pch=get_pch(meta))
    plot(p$x[,c(1,5)], col=get_col(meta), pch=get_pch(meta))
}

get_col <- function(meta){
    r.LONG <- range(meta$LONG)
    cv.LONG <- (meta$LONG - r.LONG[1])/diff(r.LONG)
    r.LAT <- range(meta$LAT)
    cv.LAT <- (meta$LAT - r.LAT[1])/diff(r.LAT)
    return(rgb(cv.LAT, 0, cv.LONG))
}
get_pch <- function(meta){
    pch=as.numeric(as.factor(meta$DATASET))
    pch=substr(meta$DATASET,1,1)
}

plot_raw <- function(datapath='sample_data/v1/input/india',
                     mcmcpath='sample_data/v1/output/india/350_run0',
		     grid=100){


diff <- as.matrix(read.table(sprintf("%s.diffs", datapath)))
meta <- read.table(sprintf("%s.meta", datapath), header=T)
o <- as.matrix(read.table(sprintf("%s.order", datapath)))
outer <- as.matrix(read.table(sprintf("%s.outer", datapath)))
coord <- as.matrix(read.table(sprintf("%s.coord", datapath)))
ipmap <- as.matrix(read.table(sprintf("%s_%s.ipmap", datapath, grid)))
demes <- as.matrix(read.table(sprintf("%s_%s.demes", datapath, grid)))



plot_original_map <- function(){
    pch=15+as.numeric(as.factor(meta$DATASET))
    plot(meta$LONG, meta$LAT, pch=get_pch(meta),
         col=get_col(meta), cex=get_cex(meta),
         asp=1)
    add.map(lwd=1)
}

plot_induced_map <- function(mcmcpath){
    plot(demes[ipmap,],
         col=NA, asp=1)
    m <- getMap('high')
    add.map(lwd=1)
    add.grid(mcmcpath)
    points(demes[ipmap,], pch=get_pch(meta),
         col=get_col(meta), cex=get_cex(meta))
}


get_cex <- function(meta, cex.samples=1.2, max.cex.samples=1.2){
    sizes <- table(ipmap)
    rel.sizes <- (sizes - min(sizes))/ diff(range(sizes))
    alpha <- as.numeric(names(rel.sizes))
    g <- rel.sizes[match(ipmap, alpha)]
                  
    cex.v <- cex.samples + max.cex.samples * g
}

plot_pca <- function(){
    p <- prcomp(diff)
    pch=15+as.numeric(as.factor(meta$DATASET))
    plot(p$x[,1:2], col=get_col(meta), pch=get_pch(meta))
}

plot_legend <- function(){
    plot.new()
    pch = get_pch(meta)
    x <- unique(cbind(pch, as.character(meta$DATASET)))
    
    legend('topright', pch=x[,1], legend=x[,2])

}


par(mfrow=c(2,2))
print('1')
plot_original_map()
print('1')
plot_induced_map(mcmcpath)
print('1')
plot_pca()
print('1')
plot_legend()
}

plot_pca_talk <- function(datapath='sample_data/v2/input/india', plotpath='test'){
    diff <- as.matrix(read.table(sprintf("%s.diffs", datapath)))
    meta <- read.table(sprintf("%s.meta", datapath), header=T)

    bitmap(paste(plotpath,'-pca.png',sep=''),type='png16m',res=200,
	   height=5,width=5,units='in')
    p <- prcomp(diff)
    pch=15+as.numeric(as.factor(meta$DATASET))
    plot(p$x[,1:2], col=get_col(meta), pch=get_pch(meta), cex=3)
    o <- as.matrix(read.table(sprintf("%s.order", datapath)))
    dev.off()

}
