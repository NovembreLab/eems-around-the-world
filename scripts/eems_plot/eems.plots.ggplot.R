mcmcpath <- paste0("../data/barrier-schemeX-nIndiv300-nSites3000-EEMS-nDemes153-simno", 1:3)
mcmcpath = "/data/eems-project/human_origins/results/africa/output/300"

plotpath <- "tmp"


require(ggplot2)
require(fields)
require(mapdata)






run <- function(mcmcpath, plotpath='tmp', longlat=T, alphaplot=T, ...){
    infiles <- c("ipmap.txt", "demes.txt", "edges.txt")
    mcmcpath <- check.files.at.path(infiles, mcmcpath)

    n_runs <- length(mcmcpath)

    if(n_runs == 0) return( 0 )
    message("processing the following EEMS output")
    message(paste0(mcmcpath,collate="\n"))
    
    dimns <- read.dimns(mcmcpath[1], longlat=longlat)


    average.eems.contours.ggplot(mcmcpath, dimns, longlat, plot.params=list(),
                                 is.mrates=T, alphaplot=alphaplot, ...)
    ggsave(paste0(plotpath,"-mrates.pdf"))
    average.eems.contours.ggplot(mcmcpath, dimns, longlat, plot.params=list(),
                                 is.mrates=F, alphaplot=alphaplot, ...)
    ggsave(paste0(plotpath,"-qrates.pdf"))
    dev.off()
}


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


average.eems.contours.ggplot <- function(mcmcpath,dimns,longlat,plot.params,
                                  is.mrates, alphaplot=T) {
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

    Z <- get.z(mcmcpath, dimns, is.mrates, longlat)
    Zmean <- Z[[1]]
    Zvar <- Z[[2]]
    

    one.eems.contour.ggplot(mcmcpath, dimns, Zmean, Zvar,
                            plot.params, is.mrates, alphaplot)
}


one.eems.contour.ggplot <- function(mcmcpath, dimns, Zmean, Zvar,
                                    plot.params, is.mrates, alphaplot=T){
    y <- rep(dimns$ymrks, each=length(dimns$xmrks))       
    x <- rep(dimns$xmrks, length(dimns$ymrks))

    Zcv <- c(sqrt(Zvar)/Zmean)
    Zprec <- 1/c(Zvar)
    Zprecn <- (Zprec-min(Zprec))/(max(Zprec)-min(Zprec))
    Zprecn <- rank(Zprecn)/length(Zprecn)
    Zcvn <- (Zcv-min(Zcv))/(max(Zcv)-min(Zcv))

    df <- data.frame(x=y, y=x, Zmean=c(Zmean), 
                     Zvar=c(Zvar), Zprecn=Zprecn,
                     Zcvn=Zcvn)


    eems_colors <- scale_fill_gradientn(colours=eems.colors)
    P <- ggplot(data=df, aes(x=x, y=y)) +
    eems_colors +
    theme(panel.background = element_rect(fill = 'white' )) +
    xlim(dimns$yrange) + ylim(dimns$xrange)

    if(alphaplot){
        P <- P + geom_tile(aes(fill=Zmean, alpha=Zprecn) ) 
    }else{
        P <- P + geom_tile(aes(fill=Zmean)) 
    }

    
    #worldmap <- map_data("worldHires")
    worldmap <- map_data("world")
    ggworld <- geom_polygon(data=worldmap, aes(x=long,  y=lat, group=group),
                            fill=NA, colour="black")
    P + ggworld
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
            nxmrks <- 100
            nymrks <- round(nxmrks/xy.asp.ratio)
        } else {
            nymrks <- 100
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
read.voronoi <- function(mcmcpath, is.mrates) {
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
standardize.rates <- function(mcmcpath,dimns,longlat,is.mrates) {
    voronoi <- read.voronoi(mcmcpath,longlat,is.mrates)
    rates <- voronoi$rates
    tiles <- voronoi$tiles
    xseed <- voronoi$xseed
    yseed <- voronoi$yseed
    Zvals <- matrix(0,dimns$nxmrks,dimns$nymrks)
    niter <- 100#length(tiles)
    count <- 0
    for (i in 1:niter) {
        print(i)
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
standardize.rates.var <- function(mcmcpath,dimns,Zmean,longlat,is.mrates) {
    voronoi <- read.voronoi(mcmcpath,longlat,is.mrates)
    rates <- voronoi$rates
    tiles <- voronoi$tiles
    xseed <- voronoi$xseed
    yseed <- voronoi$yseed
    Zvar <- matrix(0,dimns$nxmrks,dimns$nymrks)
    niter <- 100#length(tiles)
    count <- 0
    for (i in 1:niter) {
        print(i)
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
