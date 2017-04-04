require(mapdata)
require(rworldmap)
source("scripts/eems_plot/load_output.r")
source("scripts/eems_plot/worldmap.r")

default.eems.colors <- function(par='m' ) {
    if(par == 'q'){
    eems.colors <- c("#994000","#CC5800","#FF8F33","#FFAD66","#FFCA99","#FFE6CC", ## orange sequence
             #"#bbbbbb",                                                   ## white
             "#BBFFBB","#86FF86","#50FF50","#00BB00","#008600","#005000") ## green sequence
    } else {
    eems.colors <- c("#994000","#CC5800","#FF8F33","#FFAD66","#FFCA99","#FFE6CC", ## orange sequence
             #"#bbbbbb",                                                   ## white
             "#CCFDFF","#99F8FF","#66F0FF","#33E4FF","#00AACC","#007A99") ## blue sequence
    }
    return (eems.colors)
}

log.seq <- function(x,...){
    exp(seq(log(x[1]), log(x[2]), ...))
}

plot.eems.contour<- function(dimns, pts, col.range=NULL, col=1:12, 
			     n.levels=length(col), mode='median', add=F){
    mat <- matrix(NA, nrow=dimns$nxmrks, ncol=dimns$nymrks) 
    if(mode[1]=='mean'){
    mat[dimns$filter] <- colMeans(pts)
    } else if(mode[1] =='logmean'){
    mat[dimns$filter] <- exp(colMeans(log(pts)))
    } else if(mode[1] =='median'){
    mat[dimns$filter] <- apply(pts, 2, median)
    } else if(mode[1] =='cutoff'){
        spts <- log(pts) - mean(log(pts))
        mat[dimns$filter] <- 0
        mat[dimns$filter][colMeans(spts < 0) > mode[2] ] <- -1
        mat[dimns$filter][colMeans(spts > 0) > mode[2] ] <- 1
    } else {
        stop(mode)
    stop('mode not known')
    }




    if(!add){
    par(mar=c(0,0,0,0))
    plot(NA, xlim=dimns$xrange, ylim=dimns$yrange,axes=F, xlab='', ylab='',
     xaxs='i', yaxs='i', asp=1) 
    }

    if(mode[1] != 'cutoff'){
    if(is.null(col.range)){
    sq <- log.seq(range(mat,na.rm=T),length.out=n.levels+1)
    } else{
    sq <- log.seq(col.range,length.out=n.levels+1)
    sq[1] <- min(min(mat, na.rm=T), sq[1])
    sq[length(sq)] <- max(max(mat, na.rm=T), sq[length(sq)])
    }
    #sq <- seq(min(mat,na.rm=T),max(mat,na.rm=T),length.out=n.levels+1)
    print(sq)
    .filled.contour(dimns$xmrks, dimns$ymrks, mat, levels=sq, col=col)
    } else{
    .filled.contour(dimns$xmrks, dimns$ymrks, mat, levels=0:3-1.5, col=c(col[1], NA, col[2]))
    }
}

whiteout_filter <- function(dimns){
    .filled.contour(dimns$xmrks, dimns$ymrks,
                   matrix(dimns$filter, nrow=dimns$nxmrks),
                   levels=c(-1,.5, 2),
                   col=c(rgb(.8,.8,.8, .8), rgb(0,0,.12, 0)))

}

plot.key <- function(n=13, range=c(0.1,1000)){
    png('plots_talk/key_q.png', width=400, height=200 )
    xlim <- log.seq(range, length.out=n)
    xleft <- xlim[1:(n-1)]
    xright <- xlim[2:n]
    ytop <- rep(1,n-1)
    ybottom <- rep(0,n-1)

    options(scipen=999)
    plot(NA, xlim=range, ylim=0:1, xaxs='i', yaxs='i',
     xlab='', ylab='', yaxt='n', log='x', cex.axis=1.6
     )
    rect(xleft, ybottom, xright, ytop, col = default.eems.colors('q'))
    dev.off()
}

plot.eems.contour.var<- function(dimns, pts, range=NULL, col=1:12, n.levels=length(col)){
    mat <- matrix(NA, nrow=dimns$nxmrks, ncol=dimns$nymrks) 
    mat[dimns$filter] <- apply(pts,2, sd)/ colMeans(pts)

    sq <- seq(0, 3, length.out=n.levels+1)
    sq[1] <- min(min(mat, na.rm=T), sq[1])
    sq[length(sq)] <- max(max(mat, na.rm=T), sq[length(sq)])

    #sq <- seq(min(mat,na.rm=T),max(mat,na.rm=T),length.out=n.levels+1)
    .filled.contour(dimns$xmrks, dimns$ymrks, mat, levels=sq, col=col)
}

plot.empty<- function(dimns){
    mat <- matrix(NA, nrow=dimns$nxmrks, ncol=dimns$nymrks) 
    #mat[dimns$filter] <- apply(pts,2, sd)/ colMeans(pts)
    mat[!dimns$filter] <- 1

    levels <- c(0, 2)

    .filled.contour(dimns$xmrks, dimns$ymrks, mat, levels=levels,
            col='white')
}

add.map <- function(lwd=3, ...){
    plot_worldmap(lwd=lwd, ...)
}

add.grid <- function(mcmcpath, col='grey', lwd=2, ...){
    g <- read.output.graph(mcmcpath[1])
    s1 <- g$demes[g$edges[,1],]                           
    s2 <- g$demes[g$edges[,2],]                           
    segments(s1[,1], s1[,2], s2[,1], s2[,2], col=col, lwd=lwd, ...)  

}

add.samples <- function(mcmcpath, cex.samples=.8, max.cex.samples=2, pch=16, ...){
    g <- read.output.graph(mcmcpath[1])
    rel.sizes <- g$sizes / max(g$sizes)
    cex.v <- cex.samples + max.cex.samples * rel.sizes
    points(g$demes[g$alpha,1], g$demes[g$alpha,2], cex=cex.v,
       pch=pch, ...
       )
}

add.samples <- function(mcmcpath, cex.samples=.8, max.cex.samples=2, pch=16, ...){
    g <- read.output.graph(mcmcpath[1])
    rel.sizes <- g$sizes / max(g$sizes)
    cex.v <- cex.samples + max.cex.samples * rel.sizes
    points(g$demes[g$alpha,1], g$demes[g$alpha,2], cex=cex.v,
       pch=pch, ...
       )
}



