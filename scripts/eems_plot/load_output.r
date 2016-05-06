# checks if ech subfolder `paths` contains all the files in `files`.
check.files.at.path <- function(files, paths=".", strict=F){
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

#' Create coordinate object to evaluate spatial posteriors
read.dimns <- function(mcmcpath, nxmrks=NULL,nymrks=NULL) {
    outer <- scan(paste(mcmcpath,'/outer.txt',sep=''),
		what=numeric(),quiet=TRUE)
    outer <- matrix(outer,ncol=2,byrow=TRUE)

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

    require(SDMTools)
    pip <- pnt.in.poly(marks, outer)[,3]
    filter <- pip == 1

    return(list(nxmrks=nxmrks,xmrks=xmrks,xrange=c(xmin,xmax),xspan=(xmax-xmin),
                nymrks=nymrks,ymrks=ymrks,yrange=c(ymin,ymax),yspan=(ymax-ymin),
                marks=marks, filter=filter))
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

read.mfiles <- function(mcmcpath, ...){
    files <- c('/mcmcmtiles.txt','/mcmcmrates.txt', 
               '/mcmcxcoord.txt','/mcmcycoord.txt',
               '/mcmcmhyper.txt')

    read.spatial.files(mcmcpath, files, ...)
}

read.qfiles <- function(mcmcpath,...){
    files <- c('/mcmcqtiles.txt','/mcmcqrates.txt', 
               '/mcmcwcoord.txt','/mcmczcoord.txt',
               '/mcmcqhyper.txt')
    read.spatial.files(mcmcpath, files, ...)
}

read.spatial.files <- function(mcmcpath,files, ...) {
    mcmcpath <- check.files.at.path(files, mcmcpath)

    tiles <- c()
    rates <- c()
    xseed <- c()
    yseed <- c()

    for(mpath in mcmcpath){
        tiles <- c(tiles, scan(paste0(mpath, files[1]),
                               what=numeric(), quiet=T))
        rates <- c(rates, scan(paste0(mpath, files[2]),
                               what=numeric(), quiet=T))
        xseed <- c(xseed, scan(paste0(mpath, files[3]),
                               what=numeric(), quiet=T))
        yseed <- c(yseed, scan(paste0(mpath, files[4]),
                               what=numeric(), quiet=T))
    }

    tiles <- tiles[tiles>0]
    x <- cumsum(tiles)
    y <- c(0, x[-length(x)]) + 1

    rates <- lapply(1:length(x), function(i)rates[y[i]:x[i]])
    xseed <- lapply(1:length(x), function(i)xseed[y[i]:x[i]])
    yseed <- lapply(1:length(x), function(i)yseed[y[i]:x[i]])


    return(list(rates=rates,tiles=tiles,xseed=xseed,yseed=yseed))
}

if( F ){
    mcmcpath = sprintf('sample_data/v1/output/india/350_run%d/', 0:9)
    datapath = 'sample_data/v1/input/india'
    qfiles <- c('/mcmcqtiles.txt','/mcmcqrates.txt', 
               '/mcmcwcoord.txt','/mcmczcoord.txt',
               '/mcmcqhyper.txt')
    mfiles <- c('/mcmcmtiles.txt','/mcmcmrates.txt', 
               '/mcmcxcoord.txt','/mcmcycoord.txt',
               '/mcmcmhyper.txt')
}
