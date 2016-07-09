suppressPackageStartupMessages({
    library(ggplot2)
    library(magrittr)
    library(maps)
    library(gstat)
    library(RColorBrewer)
    library(sp)
    library(dplyr)
})


load_pgs_pop <- function(basename){
    pop_geo <- read.csv(sprintf("%s.pop_geo", basename))
    pop_display <- read.csv(sprintf("%s.pop_display", basename))
    pop <- inner_join(pop_geo, pop_display)
}

load_pgs_indiv <- function(basename, pop=NULL){
    indiv_label <- read.csv(sprintf("%s.indiv_label", basename))
    indiv_prov <- read.csv(sprintf("%s.indiv_prov", basename))
    indiv <- inner_join(indiv_label, indiv_prov)
    if(!is.null(pop)) indiv <- left_join(indiv, pop)

    invisible(indiv)
}

load_pgs <- function(basename){
    p <- load_pgs_pop(basename)
    i <- load_pgs_indiv(basename, p)
}

order_by_fam <- function(indiv, famfile){
    fam <- read.table(famfile, header=F)
    fam <- data.frame(sampleId=fam[,1], fam_order=1:nrow(fam))
    x <- right_join(indiv, fam) %>% arrange(fam_order) 
}

load_fam_ordered <- function(indiv_ordered, statfile, readf=read.table, ...){
    x <- readf(statfile, ...)
    cbind(indiv_ordered, x)
}

load_stat  <- function(pgs_base, famfile, Qname, stat_prefix='TESS', ...){
    meta <- load_pgs(pgs_base)
    fam_ordered <- order_by_fam(meta, famfile)
    Q <- load_fam_ordered(fam_ordered, Qname)
    tesscols <- names(Q) %in% paste0("V", 1:1000)
    names(Q)[tesscols] <- paste0(stat_prefix, 
                                 substr(names(Q)[tesscols], 2, 100))
    S <- Q %>% group_by(name)
    S_numeric <- S %>% select(which(sapply(., is.numeric)))  %>%
        summarize_each(funs(mean))
    S_categorical <- S %>% select(which(!sapply(., is.numeric)))  %>% 
        summarize_each(funs(first))
    S <- inner_join(S_categorical, S_numeric) %>% arrange(order)
    invisible(list(Q=Q, S=S))
}

load_tess <- function(..., stat_prefix='TESS'){
    if(missing(stat_prefix)){ stat_prefix='TESS'}
    load_stat(..., stat_prefix=stat_prefix)
}


interpolate_single <- function(data, boundary, PC="PC1", n=10000, idp=4, maxdist=40,...){
    data_sp <- data
    if(! 'sp' %in% class(data_sp))
        coordinates(data_sp) <- ~ longitude + latitude
    
    if(! 'sp' %in% class(boundary))
        coordinates(boundary) <- ~ x + y 
    set.seed(1)
    extrapolation_points <- spsample(Polygon(boundary), n=n, 'regular')

    res <-data.frame(coordinates(extrapolation_points),
          sapply(PC, function(P){
            fml <- as.formula(sprintf("%s ~ 1" ,P))
            x <- idw(fml, data_sp, extrapolation_points, idp=idp, maxdist=maxdist,
                     ...) 
            names(x)[names(x) == 'var1.pred'] <- P
            as.data.frame(x)[,P]
           })
          )
    coordinates(res) <- ~ x1 + x2
    res <- data.frame(res)
    names(res)[1:2] <- c('longitude', 'latitude')
    invisible(res)
}


debug <- function(){
    Q <- load_tess(pgs_base="../popres", 
                   fam='subset/europe.fam',
                   #Qname='tess/subset/europe.7_run0.Q')
                   #Qname='admixture/europe/0/europe.5.Q')
                   Qname='pca/flash_europe_dim20.pc')
    S <<- Q$S
    Q <<- Q$Q
    boundary <<- read.table("subset/europe.polygon")
    names(boundary) <<- c('x', 'y')

    pts <<- interpolate_single(S, boundary, paste0("TESS", 1:5))
}

pc_plots <- function(){
    Q <- load_tess(pgs_base="../popres", 
                   fam='subset/europe.fam',
                   #Qname='tess/subset/europe.7_run0.Q')
                   #Qname='admixture/europe/0/europe.5.Q')
                   Qname='pca/flash_europe_dim20.pc',
                   stat_prefix='PC')
    S <<- Q$S
    Q <<- Q$Q
    boundary <<- read.table("subset/europe.polygon")
    names(boundary) <<- c('x', 'y')

    pts <<- interpolate_single(S, boundary, paste0("PC", 1:5))
    png("pc_plot%d.png", width=1600, height=900)
    for(i in 1:5){
        v <- paste0("PC", i)
        single.contour(pts, v, normalize=T, col=i)
        map(add=T, lwd=2)
        text(S$longitude, S$latitude, label=S$name, col='black',
             cex=2)
    }

    dev.off()
}
tess_plots <- function(normalize=F){
    color_order <- c(3, 4, 2, 1, 5)
    Q <- load_tess(pgs_base="../popres", 
                   fam='subset/europe.fam',
                   Qname='tess/subset/europe.5_run0.Q',
                   #Qname='admixture/europe/0/europe.5.Q',
                   #Qname='pca/flash_europe_dim20.pc',
                   stat_prefix='TESS')
    S <<- Q$S
    Q <<- Q$Q
    boundary <<- read.table("subset/europe.polygon")
    names(boundary) <<- c('x', 'y')

    pts <<- interpolate_single(S, boundary, paste0("TESS", 1:5))
    png("tess_plot%d.png", width=1600, height=900)
    for(i in 1:5){
        v <- paste0("TESS", i)
        single.contour(pts, v, normalize=normalize, col=color_order[i])
        map(add=T, lwd=2)
        text(S$longitude, S$latitude, label=S$name, col='black',
             cex=2)
    }
    multi.contour(pts, stat_prefix='TESS', color_order=color_order)
    map(add=T, lwd=2)
    text(S$longitude, S$latitude, label=S$name, col='black',
         cex=2)
    dev.off()
}

adm_plots <- function(normalize=F){
    Q <- load_tess(pgs_base="../popres", 
                   fam='subset/europe.fam',
                   Qname='admixture/europe/0/europe.5.Q',
                   #Qname='pca/flash_europe_dim20.pc',
                   stat_prefix='ADM')
    S <<- Q$S
    Q <<- Q$Q
    boundary <<- read.table("subset/europe.polygon")
    names(boundary) <<- c('x', 'y')

    pts <<- interpolate_single(S, boundary, paste0("ADM", 1:5))
    png("adm_plot%d.png", width=1600, height=900)
    color_order <- c(4, 3, 1, 2, 5)
    for(i in 1:5){
        
        v <- paste0("ADM", i)
        single.contour(pts, v, normalize=normalize, col=color_order[i])
        map(add=T, lwd=2)
        text(S$longitude, S$latitude, label=S$name, col='black',
             cex=2)
    }
    multi.contour(pts, stat_prefix='ADM', color_order=color_order)
    map(add=T, lwd=2)
    text(S$longitude, S$latitude, label=S$name, col='black',
         cex=2)
    dev.off()
}

get_max_cluster <- function(pts, prefix='TESS'){
    q <- pts %>% select(starts_with(prefix))     
    max_cluster <- apply(q, 1, which.max)     
    max_cluster_val <- apply(q, 1, max)       
    return(data.frame(latitude=pts$latitude, longitude=pts$longitude, 
                      max_cluster, max_cluster_val))
}


filter_nonmax <- function(pts, prefix='TESS'){
    q <- pts %>% select(starts_with(prefix))     
    y <- t(apply(q, 1, function(x) x * (x == max(x))))
    y[y==0] <- NA
    return(data.frame(latitude=pts$latitude, longitude=pts$longitude, 
        y))
}


get_cols <- function(n, pal="Set1", n_levels=10){
    n <- max(3, n)
    pal <- brewer.pal(n, pal)
    lapply(pal, function(col)colorRampPalette(c('white', col))(n_levels))
}


multi.contour <- function(pts, n_levels=10, stat_prefix='ADM', color_order=NULL, ...){
    x <- sort(unique(pts$longitude))
    y <- sort(unique(pts$latitude))
    xy <- expand.grid(longitude=x, latitude=y)

    max_cluster <- filter_nonmax(pts, prefix=stat_prefix)
    xyz <- left_join(xy, max_cluster)

    factors <- xyz %>% select(starts_with(stat_prefix)) %>% names()
    n_fac <- length(factors)
    if(missing(color_order))color_order <- 1:n_fac

    cols <- get_cols(n_fac, n_levels=n_levels)
    plot(NA, xlim=range(x), ylim=range(y))
    for(i in 1:n_fac){
        cur_fac <- factors[i]
        print(cur_fac)
        .filled.contour(x, y, matrix(xyz[,cur_fac], ncol=length(y)),
                        levels=0:n_levels/n_levels,
                        col=cols[[color_order[i]]])

    }
}

single.contour <- function(pts, stat, n_levels=10, normalize=F, col=1, ...){
    x <- sort(unique(pts$longitude))
    y <- sort(unique(pts$latitude))
    xy <- expand.grid(longitude=x, latitude=y)

    xyz <- left_join(xy, pts)

    factor <- xyz %>% select_(as.name(stat)) %>% unlist()
    if(normalize) factor <- (factor - min(factor, na.rm=T)) / diff(range(factor, na.rm=T))  

    n_fac <- 8

    cols <- get_cols(n_fac, n_levels=n_levels)
    plot(NA, xlim=range(x), ylim=range(y))
        .filled.contour(x, y, matrix(factor, ncol=length(y)),
                        levels=0:n_levels/n_levels,
                        col=cols[[col]])

}
