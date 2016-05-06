cex.pts <- 2
mcmcpath <- sprintf('sample_data/v2/output/africa/350_run%d/', 0:9)              
dimns <- read.dimns(mcmcpath[1], 100, 100)

b <- read.table("sample_data/v2/input/africa.meta", header=T)
g <- read.table("sample_data/v2/input/africa_100.demes", header=F)
h <- read.table("sample_data/v2/input/africa_100.ipmap", header=F)

png(file='plots_talk/intro2-%d.png', width=7, height=8, res=100, units='in')
par(mar=c(0,0,0,0))
plot(NA, xlim=dimns$xrange, ylim=dimns$yrange, axes=0,
     xlab='', ylab='', mar=c(0,0,0,0), xaxs='i', yaxs='i',
     asp=1)
add.map(col='lightgray', lwd=.1)


plot(NA, xlim=dimns$xrange, ylim=dimns$yrange, axes=0,
     xlab='', ylab='', mar=c(0,0,0,0), xaxs='i', yaxs='i',
     asp=1)
add.map(col='lightgray', lwd=.1)
points(b$LONG, b$LAT, col='#00000011', cex=cex.pts, pch=16)
points(b$LONG, b$LAT, col='#000000', cex=cex.pts, pch=1)


plot(NA, xlim=dimns$xrange, ylim=dimns$yrange, axes=0,
     xlab='', ylab='', mar=c(0,0,0,0), xaxs='i', yaxs='i',
     asp=1)
add.map(col='lightgray', lwd=.1)
points(b$LONG, b$LAT, col='#00000011', cex=cex.pts, pch=16)
points(b$LONG, b$LAT, col='#000000', cex=cex.pts, pch=1)
add.grid(mcmcpath, col='blue')


plot(NA, xlim=dimns$xrange, ylim=dimns$yrange, axes=0,
     xlab='', ylab='', mar=c(0,0,0,0), xaxs='i', yaxs='i',
     asp=1)
add.map(col='lightgray', lwd=.1)
points(b$LONG, b$LAT, col='#00000011', cex=cex.pts, pch=16)
points(b$LONG, b$LAT, col='#000000', cex=cex.pts, pch=1)
add.grid(mcmcpath, col='blue')
points(g[unique(h[,1]),], col='red', cex=cex.pts, pch=16)

plot(NA, xlim=dimns$xrange, ylim=dimns$yrange, axes=0,
     xlab='', ylab='', mar=c(0,0,0,0), xaxs='i', yaxs='i',
     asp=1)
add.map(col='lightgray', lwd=.1)
points(b$LONG, b$LAT, col='#00000011', cex=cex.pts, pch=16)
points(b$LONG, b$LAT, col='#000000', cex=cex.pts, pch=1)
cols <- default.eems.colors()[sample(1:12, 1000, replace=T)] 
add.grid(mcmcpath, col=cols, lwd=3) 
points(g[unique(h[,1]),], col='red', cex=cex.pts, pch=16)

read.voronoi <- function(mcmcpath,longlat,is.mrates) {
    if (is.mrates) {
        rates <- scan(paste(mcmcpath,'/mcmcmrates.txt',sep=''),what=numeric(),quiet=TRUE)
        tiles <- scan(paste(mcmcpath,'/mcmcmtiles.txt',sep=''),what=numeric(),quiet=TRUE)
        xseed <- scan(paste(mcmcpath,'/mcmcxcoord.txt',sep=''),what=numeric(),quiet=TRUE)
        yseed <- scan(paste(mcmcpath,'/mcmcycoord.txt',sep=''),what=numeric(),quiet=TRUE)
    } else {
        rates <- scan(paste(mcmcpath,'/mcmcqrates.txt',sep=''),what=numeric(),quiet=TRUE)
        tiles <- scan(paste(mcmcpath,'/mcmcqtiles.txt',sep=''),what=numeric(),quiet=TRUE)
        xseed <- scan(paste(mcmcpath,'/mcmcwcoord.txt',sep=''),what=numeric(),quiet=TRUE)
        yseed <- scan(paste(mcmcpath,'/mcmczcoord.txt',sep=''),what=numeric(),quiet=TRUE)
    }
    if (!longlat) {
        tempi <- xseed
        xseed <- yseed
        yseed <- tempi
    }
    rates <- log10(rates)
    return(list(rates=rates,tiles=tiles,xseed=xseed,yseed=yseed))
}
voronoi.diagram <- function(mcmcpath,dimns,longlat,plot.params,mcmc.iters=NULL,is.mrates=TRUE) {
    mcmcpath <- mcmcpath[1]
    writeLines('Plotting Voronoi tessellation of estimated effective rates')
    writeLines(mcmcpath)
    voronoi <- read.voronoi(mcmcpath,longlat,is.mrates)
    rates <- voronoi$rates
    tiles <- voronoi$tiles
    xseed <- voronoi$xseed
    yseed <- voronoi$yseed
    niters <- length(tiles)
    eems.colors <- plot.params$eems.colors 
    num.levels <- length(eems.colors)
    if (is.mrates) {
        main.title <- 'Effective migration rates m'
        eems.levels <- seq(min(rates), max(rates), length.out=13)
	print(eems.levels)
    } else {
        main.title <- 'Effective diversity rates q'
        eems.levels <- eems.colscale(rates,num.levels,plot.params$q.colscale)
    }
    if (is.null(mcmc.iters)) {
        mcmc.iters <- seq(niters)
    }
    count <- 0
    for (i in 1:niters) {
        now.tiles <- tiles[i]
        now.rates <- rates[(count+1):(count+now.tiles)]
        now.xseed <- xseed[(count+1):(count+now.tiles)]
        now.yseed <- yseed[(count+1):(count+now.tiles)]
        count <- count + now.tiles
        if (!(i %in% mcmc.iters)) { next }
        ## Standardize the log-transformed rates, without taking into account
        ## the relative size of the tiles (this is hard to do without a grid)
        now.rates <- now.rates - mean(now.rates)
        L <- length(eems.levels)
        indices <- which(now.rates<eems.levels[1])
        now.rates[indices] <- 0.999*eems.levels[1]
        indices <- which(now.rates>eems.levels[L])
        now.rates[indices] <- 0.999*eems.levels[L]
        now.seeds <- cbind(now.xseed,now.yseed)
        now.colors <- character( )
        now.main.title <- paste(main.title," : iteration ",i," (after burn-in and thinning)",sep="")
        if (!plot.params$add.title) { now.main.title <- "" }
        plot(0,0,type="n",xlim=dimns$xrange,ylim=dimns$yrange,asp=1,
             axes=FALSE,xlab="",ylab="",main=now.main.title)
        if (now.tiles==1) {
            ## There is only one tile
            tile.color <- eems.colors[round(L/2)]
            now.colors <- c(now.colors,tile.color)
            polygon(dimns$xrange,dimns$yrange,col=tile.color,border=FALSE)
        } else {
            ## Plot each tile in turn (as a polygon)
            Voronoi <- deldir::deldir(now.xseed,now.yseed,rw=c(dimns$xrange,dimns$yrange))
            tilelist <- deldir::tile.list(Voronoi)
            for (c in 1:now.tiles) {
                tile.color <- eems.colors[ max((1:L)[eems.levels<now.rates[c]]) ]
                now.colors <- c(now.colors,tile.color)
                polygon(tilelist[[c]]$x,tilelist[[c]]$y,col=tile.color,border=FALSE)
            }
            filled.contour.axes(mcmcpath,longlat,plot.params)
        }
        if (plot.params$add.seeds) {
            points(now.seeds,pch=plot.params$pch.seeds,cex=plot.params$cex.seeds,col=plot.params$col.seeds)
        }
    }
    return(list(colors=eems.colors,levels=eems.levels))    
}

filled.contour.axes <- function(mcmcpath,longlat,plot.params) {
    if (is.null(plot.params$proj.in)) {
        filled.contour.axes.proj.unknown(mcmcpath,longlat,plot.params)
    } else {
        filled.contour.axes.proj.known(mcmcpath,longlat,plot.params)
    }
}
filled.contour.axes.proj.unknown <- function(mcmcpath,longlat,plot.params) {
    graph <- read.graph(mcmcpath,longlat)
    if (plot.params$add.grid) {
        segments <- list()
        for (e in 1:nrow(graph$edges)) {
            segments[[e]] <- sp::Line(graph$demes[graph$edges[e,],])
        }
        segments <- sp::SpatialLines(list(Lines(segments,ID="a")))
        lines(segments,col=plot.params$col.grid,lwd=plot.params$lwd.grid)
    }
    if (plot.params$all.demes) {
        all.demes <- sp::SpatialPoints(graph$demes)
        cex.points <- plot.params$min.cex.demes
        points(all.demes,col=plot.params$col.demes,pch=plot.params$pch.demes,cex=cex.points)
    } else if (plot.params$add.demes) {
        observed.demes <- sp::SpatialPoints(graph$demes[graph$alpha,])
        cex.points <- plot.params$min.cex.demes
        if (min(graph$sizes) < max(graph$sizes)) {
            cex.points <- cex.points +
                (plot.params$max.cex.demes - plot.params$min.cex.demes) *
                    (graph$sizes - min(graph$sizes)) / (max(graph$sizes) - min(graph$sizes))
        }
        points(observed.demes,col=plot.params$col.demes,pch=plot.params$pch.demes,cex=cex.points)
    }
}
read.graph <- function(path,longlat) {
    eems.output <- NULL
    if (file.exists(paste(path,'/demes.txt',sep='')) &&
        file.exists(paste(path,'/ipmap.txt',sep='')) &&
        file.exists(paste(path,'/outer.txt',sep=''))) {
        eems.output <- TRUE
    } else if (file.exists(paste(path,'.coord',sep='')) &&
               file.exists(paste(path,'.diffs',sep='')) &&
               file.exists(paste(path,'.outer',sep=''))) {
        eems.output <- FALSE
    }
    if (is.null(eems.output)) {
        stop(paste(path,' is neither a datapath nor a mcmcpath.',sep=''))
    }
    if (eems.output) {
        ## Read the assigned sample coordinates
        ipmap <- scan(paste(path,'/ipmap.txt',sep=''),what=numeric(),quiet=TRUE)
        demes <- scan(paste(path,'/demes.txt',sep=''),what=numeric(),quiet=TRUE)
        outer <- scan(paste(path,'/outer.txt',sep=''),what=numeric(),quiet=TRUE)
        demes <- matrix(demes,ncol=2,byrow=TRUE)
        outer <- matrix(outer,ncol=2,byrow=TRUE)
        edges <- read.edges(path)
    } else {
        ## Read the original sample coordinates
        coord <- scan(paste(path,'.coord',sep=''),what=numeric(),quiet=TRUE)
        outer <- scan(paste(path,'.outer',sep=''),what=numeric(),quiet=TRUE)
        coord <- matrix(coord,ncol=2,byrow=TRUE)
        outer <- matrix(outer,ncol=2,byrow=TRUE)
        edges <- NULL
        ## In this case each sample is its own "deme"
        ## Samples with exactly the same location are overplotted
        #ipmap <- seq(nrow(coord))
        #demes <- coord
        ## In this case the two sampling coordinates are combined
        ## to define a "deme", with the maximum possible precision
        ipmap <- factor(paste(coord[,1],coord[,2],sep="x"))
        demes <- levels(ipmap)
        index <- match(demes,ipmap)
        o <- length(index)
        demes <- coord[index,]
        ipmap <- (1:o)[ipmap]
        ## "Close" the outline if the first row is not the same as the last row
        if (sum(head(outer,1) != tail(outer,1))) {
            outer = rbind(outer, head(outer,1))
        }
    }
    if (!longlat) {
        demes <- demes[,c(2,1)]
        outer <- outer[,c(2,1)]
    }
    sizes <- table(ipmap)
    alpha <- as.numeric(names(sizes))
    sizes <- as.numeric(sizes)
    return(list(ipmap=ipmap,demes=demes,edges=edges,alpha=alpha,sizes=sizes,outer=outer))
}

plot.params=list(add.title=F, add.grid=F, add.demes=T, all.demes=T, add.seeds=T, eems.colors=default.eems.colors())
voronoi.diagram(mcmcpath, dimns, T, mcmc.iters=c(1), plot.params=plot.params)
add.grid(mcmcpath)
add.map()
plot.empty(dimns)
plot.empty(dimns)
plot.empty(dimns)

voronoi.diagram(mcmcpath, dimns, T, mcmc.iters=c(3), plot.params=plot.params)
add.grid(mcmcpath)
add.map()
plot.empty(dimns)
plot.empty(dimns)
plot.empty(dimns)

mfiles <- read.mfiles(mcmcpath)
pts <- compute.pts(dimns, mfiles)
col.range=c(0.1, 1000)
plot(NA, xlim=dimns$xrange, ylim=dimns$yrange, axes=0,
     xlab='', ylab='', mar=c(0,0,0,0), xaxs='i', yaxs='i',
     asp=1)
plot.eems.contour(dimns, pts, col=default.eems.colors('m'),
		  mode='logmean', col.range=col.range, add=T)

points(b$LONG, b$LAT, col='#00000011', cex=cex.pts, pch=16)
points(b$LONG, b$LAT, col='#000000', cex=cex.pts, pch=1)
add.map()
dev.off()
