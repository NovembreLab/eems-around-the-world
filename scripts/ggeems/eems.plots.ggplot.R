require(ggplot2)
require(fields)
require(mapdata)
options(bitmapType='cairo')


#RES <- 40 #the number of tiles along each axis.


get.z <- function(mcmcpath, dimns, is.mrates, longlat=F){
    niter <- 0
    Zmean <- matrix(0,dimns$nxmrks,dimns$nymrks)
    for (path in mcmcpath) {
        rslt <- standardize.rates(path,dimns,longlat,is.mrates)
        niter <- niter + rslt$niter
        Zmean <- Zmean + rslt$Zvals
    }
    Zmean <- Zmean/niter

    Zvar <- matrix(0,dimns$nxmrks,dimns$nymrks)
    for (path in mcmcpath) {
        rslt <- standardize.rates.var(path,dimns,Zmean,longlat,is.mrates)
        Zvar <- Zvar + rslt$Zvar
    }
    Zvar <- Zvar/(niter-1)

    return(list(mean=Zmean, var=Zvar))
}

get.sign <- function(mcmcpath, dimns, is.mrates, longlat=F){
    niter <- 0
    Zpos <- matrix(0,dimns$nxmrks,dimns$nymrks)
    Zneg <- matrix(0,dimns$nxmrks,dimns$nymrks)
    for (path in mcmcpath) {
        rslt <- standardize.rates.sign(path,dimns,longlat,is.mrates)
        niter <- niter + rslt$niter
        Zpos <- Zpos + rslt$Zpos
        Zneg <- Zneg + rslt$niter - rslt$Zpos
    }
    Zpos <- Zpos/niter
    Zneg <- Zneg/niter

    Z <- matrix(0.000,dimns$nxmrks,dimns$nymrks)
    Z[Zpos > .95] <- 1.99
    Z[Zneg > .95] <- -1.99

    print(Zpos)
    return(Z)
}


average.eems.contours.ggplot <- function(P, mcmcpath,dimns,
                                  is.mrates, signplot=F, ...) {
    if (is.mrates) {
        message('Plotting effective migration rates m : posterior mean and variance')
        files <- c('/mcmcmtiles.txt','/mcmcmrates.txt', 
                   '/mcmcxcoord.txt','/mcmcycoord.txt')
    } else {
        message('Plotting effective diversity rates q : posterior mean and variance')
        files <- c('/mcmcqtiles.txt','/mcmcqrates.txt', 
                   '/mcmcwcoord.txt','/mcmczcoord.txt')
    }
    mcmcpath <- check.files.at.path(files, mcmcpath)

    n_runs <- length(mcmcpath)
    if (n_runs==0) { return(0) }

    if(signplot){
        Z <- get.sign(mcmcpath, dimns, is.mrates, T)
        saveRDS(Z, "z.rds")
        return(add.one.eems.contour.ggplot(P, mcmcpath, dimns, Z, Zvar=NULL,
                                 is.mrates, ...))
    }else{
        Z <- get.z(mcmcpath, dimns, is.mrates, T)
        Zmean <- Z[[1]]
        Zvar <- Z[[2]]
        return(add.one.eems.contour.ggplot(P, mcmcpath, dimns, Zmean, Zvar,
                                 is.mrates, ...))
    }
}


add.one.eems.contour.ggplot <- function(P, mcmcpath, dimns, Zmean, Zvar,
                                     is.mrates, alpha_limits=c(0,.1), 
                                     alpha_null=0.6, ...){
    y <- rep(dimns$ymrks, each=length(dimns$xmrks))       
    x <- rep(dimns$xmrks, length(dimns$ymrks))

    #Zcv <- c(sqrt(Zvar)/Zmean)
    #Zprec <- 1/c(Zvar)
    #Zprecn <- (Zprec-min(Zprec))/(max(Zprec)-min(Zprec))
    #Zprecn <- rank(Zprecn)/length(Zprecn)
    #Zcvn <- (Zcv-min(Zcv))/(max(Zcv)-min(Zcv))

    df <- data.frame(x=y, y=x, Zmean=c(Zmean), 
                     filter=dimns$filter)
    dff <- df[!df$filter,]
    dff$alpha <- alpha_null
    dff <<- dff

    df <- df[df$filter,]
    df$Zmean <- df$Zmean - mean(df$Zmean)

    df$Zmean[df$Zmean > 2] <- 2
    df$Zmean[df$Zmean < (-2)] <- -2

    n_cuts <- max(min(101, length(unique(df$Zmean))), 2)
    print(n_cuts)
    df$alpha <- cut(df$Zmean, n_cuts)
    alpha_scale <- scale_alpha_manual(labels=levels(df$alpha), 
                                      values=alpha, guide='none',
                                      palette=function(n){
                                          nhalf = n %/% 2
                                          s <- seq(alpha_limits[2], 
                                              alpha_limits[1], 
                                              length.out=nhalf)
                                          c(s, alpha_limits[1], rev(s))
                                      })

    #rescaling for debug
    #df$y <- 8 + (df$y - min(df$y))/diff(range(df$y))*2.3
    #df$x <- 38.85 + (df$x - min(df$x))/diff(range(df$x))*2.53


    if(is.mrates){
	eems_colors <- scale_fill_gradientn(colours=eems.colors,
					    name="M", limits=c(-2,2))
    } else {
	eems_colors <- scale_fill_gradientn(colours=eems.colors,
					    name="q")
    }
    #P <- ggplot(data=df, aes(x=x, y=y)) +
    tiles <- P + geom_tile(data=df, aes(x=y, y=x, fill=Zmean, alpha=alpha) ) +
	eems_colors + alpha_scale 
    tiles <- tiles + geom_tile(data=dff, aes(x=y, y=x),
			       alpha=alpha_null, fill='white', color=NA)
			    
    return(tiles)
}

check.files.at.path <- function(files, paths=".", strict=F){
    # checks if ech subfolder `paths` contains all the files in `files`.
    file_paths <- c(outer(paths, files, paste, sep=.Platform$file.sep))
    file_exist <- file.exists(file_paths)
    if( all(file_exist) ) return( paths )
    
    print(paste0(file_paths[!file_exist], " not found", collate="\n") )

    if( strict ){
        stop( )
    }
    
    f <- matrix(file_exist, nrow=length(paths))
    return(paths[ rowSums(f) == ncol(f)])
}


read.dimns <- function(mcmcpath,longlat,nxmrks=NULL,nymrks=NULL) {
    outer <- scan(paste(mcmcpath,'/outer.txt',sep=''),what=numeric(),quiet=TRUE)
    outer <- matrix(outer,ncol=2,byrow=TRUE)
    if (!longlat) {
        outer <- outer[,c(2,1)]
    }
    xmin <- min(outer[,1])
    xmax <- max(outer[,1])
    ymin <- min(outer[,2])
    ymax <- max(outer[,2])
    ## Choose the number of interpolation in each direction
    if (is.null(nxmrks)&&is.null(nymrks)) {
        xy.asp.ratio <- (xmax-xmin)/(ymax-ymin)
        if (xy.asp.ratio>1) {
            nxmrks <- RES 
            nymrks <- round(nxmrks/xy.asp.ratio)
        } else {
            nymrks <- RES
            nxmrks <- round(nymrks*xy.asp.ratio)
        }
    }
    ## The interpolation points are equally spaced
    xmrks <- seq(xmin,xmax,length=nxmrks)
    ymrks <- seq(ymin,ymax,length=nymrks)
    marks <- cbind(rep(xmrks,times=nymrks),rep(ymrks,each=nxmrks))
    ## Experimenting with the pixmap package to create pixel maps of estimated rates
    marks.pixmap.order <- cbind(rep(xmrks,each=nymrks),rep(rev(ymrks),times=nxmrks))
    return(list(nxmrks=nxmrks,xmrks=xmrks,xrange=c(xmin,xmax),xspan=(xmax-xmin),
                nymrks=nymrks,ymrks=ymrks,yrange=c(ymin,ymax),yspan=(ymax-ymin),
                marks=marks,marks.pixmap.order=marks.pixmap.order))
}
read.dimns <- function(mcmcpath, longlat=T, nxmrks=NULL,nymrks=NULL) {
    outer <- scan(paste(mcmcpath,'/outer.txt',sep=''),
		what=numeric(),quiet=TRUE)
    outer <- matrix(outer,ncol=2,byrow=TRUE)
    if (!longlat) {
        outer <- outer[,c(2,1)]
    }

    xmin <- min(outer[,1])
    xmax <- max(outer[,1])
    ymin <- min(outer[,2])
    ymax <- max(outer[,2])
    ## Choose the number of interpolation in each direction
    if (is.null(nxmrks)&&is.null(nymrks)) {
        xy.asp.ratio <- (xmax-xmin)/(ymax-ymin)
        if (xy.asp.ratio>1) {
            nxmrks <- RES
            nymrks <- round(nxmrks/xy.asp.ratio)
        } else {
            nymrks <- RES
            nxmrks <- round(nymrks*xy.asp.ratio)
        }
    }
    ## The interpolation points are equally spaced
    xmrks <- seq(xmin,xmax,length=nxmrks)
    ymrks <- seq(ymin,ymax,length=nymrks)
    marks <- cbind(rep(xmrks,times=nymrks),rep(ymrks,each=nxmrks))

    require(SDMTools)
    pip <- pnt.in.poly(marks, outer)[,3]
    filter <- pip == 1

    return(list(nxmrks=nxmrks,xmrks=xmrks,xrange=c(xmin,xmax),xspan=(xmax-xmin),
                nymrks=nymrks,ymrks=ymrks,yrange=c(ymin,ymax),yspan=(ymax-ymin),
                marks=marks, filter=filter))
}
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
compute.contour.vals <- function(dimns,seeds,rates,use.weighted.mean=TRUE) {
    ## Here 'seeds' stores the generator seeds of a Voronoi tessellation
    ## and 'rates' stores the log10-transformed rates of the tiles.
    ## If there are C seeds in the partition, then 'seeds' is a matrix
    ## with C rows and 2 columns and 'rates' is a vector with C elements
    distances <- rdist(dimns$marks,seeds)
    closest <- apply(distances,1,which.min)
    if (use.weighted.mean) {
        zvals <- matrix(rates[closest],dimns$nxmrks,dimns$nymrks,byrow=FALSE)
        zvals <- zvals - mean(zvals)
    } else {
        rates <- rates - mean(rates)
        zvals <- matrix(rates[closest],dimns$nxmrks,dimns$nymrks,byrow=FALSE)
    }
    return(zvals)
}
compute.contour.vals.sign <- function(dimns,seeds,rates,use.weighted.mean=TRUE) {
    ## Here 'seeds' stores the generator seeds of a Voronoi tessellation
    ## and 'rates' stores the log10-transformed rates of the tiles.
    ## If there are C seeds in the partition, then 'seeds' is a matrix
    ## with C rows and 2 columns and 'rates' is a vector with C elements
    distances <- rdist(dimns$marks,seeds)
    closest <- apply(distances,1,which.min)
    if (use.weighted.mean) {
        zvals <- matrix(rates[closest],dimns$nxmrks,dimns$nymrks,byrow=FALSE)
        zvals <- zvals - mean(zvals)
    } else {
        rates <- rates - mean(rates)
        zvals <- matrix(rates[closest],dimns$nxmrks,dimns$nymrks,byrow=FALSE)
    }
    return(zvals>0)
}
standardize.rates <- function(mcmcpath,dimns,longlat,is.mrates) {
    voronoi <- read.voronoi(mcmcpath,longlat,is.mrates)
    rates <- voronoi$rates
    tiles <- voronoi$tiles
    xseed <- voronoi$xseed
    yseed <- voronoi$yseed
    Zvals <- matrix(0,dimns$nxmrks,dimns$nymrks)
    niter <- min(100, length(tiles))
    count <- 0
    for (i in 1:niter) {
        now.tiles <- tiles[i]
        now.rates <- rates[(count+1):(count+now.tiles)]
        now.xseed <- xseed[(count+1):(count+now.tiles)]
        now.yseed <- yseed[(count+1):(count+now.tiles)]
        now.seeds <- cbind(now.xseed,now.yseed)
        zvals <- compute.contour.vals(dimns,now.seeds,now.rates)
        Zvals <- Zvals + zvals
        count <- count + now.tiles
    }
    return(list(Zvals=Zvals,niter=niter))
}

standardize.rates.sign <- function(mcmcpath,dimns,longlat,is.mrates) {
    voronoi <- read.voronoi(mcmcpath,longlat,is.mrates)
    rates <- voronoi$rates
    tiles <- voronoi$tiles
    xseed <- voronoi$xseed
    yseed <- voronoi$yseed
    Zpos <- matrix(0,dimns$nxmrks,dimns$nymrks)
    niter <- min(100, length(tiles))
    count <- 0
    for (i in 1:niter) {
        now.tiles <- tiles[i]
        now.rates <- rates[(count+1):(count+now.tiles)]
        now.xseed <- xseed[(count+1):(count+now.tiles)]
        now.yseed <- yseed[(count+1):(count+now.tiles)]
        now.seeds <- cbind(now.xseed,now.yseed)
        zvals <- compute.contour.vals.sign(dimns,now.seeds,now.rates)
        Zpos <- Zpos + zvals
        count <- count + now.tiles
    }
    return(list(Zpos=Zpos,niter=niter))
}
standardize.rates.var <- function(mcmcpath,dimns,Zmean,longlat,is.mrates) {
    voronoi <- read.voronoi(mcmcpath,longlat,is.mrates)
    rates <- voronoi$rates
    tiles <- voronoi$tiles
    xseed <- voronoi$xseed
    yseed <- voronoi$yseed
    Zvar <- matrix(0,dimns$nxmrks,dimns$nymrks)
    niter <- min(100, length(tiles))
    count <- 0
    for (i in 1:niter) {
        now.tiles <- tiles[i]
        now.rates <- rates[(count+1):(count+now.tiles)]
        now.xseed <- xseed[(count+1):(count+now.tiles)]
        now.yseed <- yseed[(count+1):(count+now.tiles)]
        now.seeds <- cbind(now.xseed,now.yseed)
        zvals <- compute.contour.vals(dimns,now.seeds,now.rates)
        Zvar <- Zvar + (zvals - Zmean)^2
        count <- count + now.tiles
    }
    return(list(Zvar=Zvar,niter=niter))
}
eems.colors <- c("#994000","#CC5800","#FF8F33","#FFAD66","#FFCA99","#FFE6CC", ## orange sequence
                 "#FFFFFF",                                                   ## white
                 "#CCFDFF","#99F8FF","#66F0FF","#33E4FF","#00AACC","#007A99") ## blue sequence

gg_add_samples <- function(mcmcpath, cex.samples=.8, max.cex.samples=1.6, pch=16, ...){
    g <- read.output.graph(mcmcpath[1])
    rel.sizes <- g$sizes / max(g$sizes)
    cex.v <- cex.samples + max.cex.samples * rel.sizes
    df=data.frame(x=g$demes[g$alpha,1], y=g$demes[g$alpha,2],
                  size=cex.v*3)
    return(geom_point(data=df, aes(x=x,y=y, size=size))
           )
}

#adds sample with xy-coords switched 
gg_add_samples_wrong <- function(mcmcpath, cex.samples=.8, max.cex.samples=2, pch=16, ...){
    g <- read.output.graph(mcmcpath[1])
    rel.sizes <- g$sizes / max(g$sizes)
    cex.v <- cex.samples + max.cex.samples * rel.sizes
    df=data.frame(x=g$demes[g$alpha,2], y=g$demes[g$alpha,1],
                  size=cex.v*2.5)
    return(geom_point(data=df, aes(x=x,y=y, size=size))
           )
}
#adds sample with xy-coords switched  and blue
gg_add_samples_wrong_blue <- function(mcmcpath, cex.samples=.8, max.cex.samples=2, pch=16, ...){
    g <- read.output.graph(mcmcpath[1])
    rel.sizes <- g$sizes / max(g$sizes)
    cex.v <- cex.samples + max.cex.samples * rel.sizes
    df=data.frame(x=g$demes[g$alpha,2], y=g$demes[g$alpha,1],
                  size=cex.v*2.5)
    return(geom_point(data=df, aes(x=x,y=y, size=size), color='#fec907')
    #return(geom_point(data=df, aes(x=x,y=y, size=size), color='darkblue')
    #return(geom_point(data=df, aes(x=x,y=y, size=size), color='darkblue')
           )
}

read.output.graph <- function(path) {
    ipmap <- scan(paste(path,'/ipmap.txt',sep=''),what=numeric(),quiet=TRUE)
    demes <- scan(paste(path,'/demes.txt',sep=''),what=numeric(),quiet=TRUE)
    outer <- scan(paste(path,'/outer.txt',sep=''),what=numeric(),quiet=TRUE)

    demes <- matrix(demes,ncol=2,byrow=TRUE)
    outer <- matrix(outer,ncol=2,byrow=TRUE)
    edges <- read.edges(path)

    sizes <- table(ipmap)
    alpha <- as.numeric(names(sizes))
    sizes <- as.numeric(sizes)
    return(list(ipmap=ipmap,demes=demes,edges=edges,alpha=alpha,sizes=sizes,outer=outer))
}
read.edges <- function(mcmcpath) {
    edges <- read.table(paste(mcmcpath,'/edges.txt',sep=''),colClasses=numeric())
    edges <- as.matrix(edges)
    if (ncol(edges)==6) {
        ## Convert the old format to the new format
        edges0 <- edges
        edges <- matrix(0,nrow=sum(edges0>0),ncol=2)
        nv <- nrow(edges0)
        nn <- ncol(edges0)
        nodes <- 1:nv
        e <- 0
        for (a in 1:nv) {
        for (i in 1:nn) {
            b <- edges0[a,i]
            if (b %in% nodes) {
                e <- e + 1
                edges[e,1] = a
                edges[e,2] = b
            }
        } }
    }
    return(edges)
}

f <- function(x, alpha) adjustcolor(x, alpha.f=alpha)
alpha = c(seq(.66, .0, length.out=50),
	  rep(0, 1),
	  seq(.0, .66, length.out=50))
	    

