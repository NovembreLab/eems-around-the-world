#library(rgdal)
#library(jpeg)
require(fields)
source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/eems.plots.ggplot.R")


RES=20

medi_folder <- '../../../../results/Hussein/eemsplots/3-25-16/'
mcmcpath_medi <- sprintf("%s/medit/runs_w_basque/run_%s/", medi_folder, 0:5)

add_eems_overlay <- function(P, mcmcpath, is.mrates=T, ...){
    infiles <- c("ipmap.txt", "demes.txt", "edges.txt")
    mcmcpath <- check.files.at.path(infiles, mcmcpath)

    n_runs <- length(mcmcpath)

    if(n_runs == 0) return( 0 )
    message("processing the following EEMS output")
    message(paste0(mcmcpath,collate="\n"))
    
    dimns <- read.dimns(mcmcpath[1])


    P <- average.eems.contours.ggplot(P, mcmcpath, dimns, 
                                 is.mrates=is.mrates, ...) #+
    #ggsave(paste0(plotpath,"-mrates.pdf"))
    #average.eems.contours.ggplot(P, mcmcpath, dimns, longlat, plot.params=list(),
    #                             is.mrates=F, alphaplot=alphaplot, ...)
    #ggsave(paste0(plotpath,"-qrates.pdf"))
    #dev.off()
}

combine_map <- function(bbox, zoom=6){
    bbox1 <- bbox
    bbox1['right'] <- 180-1e-10
    bbox2 <- bbox
    bbox2['left'] <- -180
    bbox2['right'] <- bbox['right'] - 360
    m1 <- get_stamenmap(bbox=bbox1, zoom=zoom);   
    m2 <- get_stamenmap(bbox=bbox2, zoom=zoom);   
    attr(m2, 'bb')[c(2,4)] <-  attr(m2, 'bb')[c(2,4)] + 359.99

    print("done loading  map")

    a = ggmap(m1) + inset_ggmap(m2)
}

shifted_map <- function(bbox, zoom=6){
    bbox['left'] <- bbox['left'] - 360
    bbox['right'] <- bbox['right'] - 360
    m2 <- get_stamenmap(bbox=bbox, zoom=zoom);   
    attr(m2, 'bb')[c(2,4)] <-  attr(m2, 'bb')[c(2,4)] + 360  
    a = ggmap(m2)
}


make_map <- function(mcmcpath, zoom=6, is.mrates=T, fancy_proj=F){
    boundary <- read.table(sprintf("%s/outer.txt", mcmcpath[1]))
    bbox <- c(left=min(boundary[1]), right=max(boundary[1]),
              bottom=min(boundary[2]), top=max(boundary[2]))
    bbox['top'] <- pmin(bbox['top'], 80)

    if(!fancy_proj){
	library(ggmapcustom)
	if(bbox['right'] > 180 && bbox['left'] > 180){
	    print("shifted map")
	    a = shifted_map(bbox, zoom)
	} else if(bbox['right'] > 180 ){
	    print("combined map")
	    a = combine_map(bbox, zoom)
	} else {
	    map <- get_stamenmap(bbox=bbox, zoom=zoom);                     
	    print("done loading  map")
	    a=ggmap(map)                                                                            
	}
    } else {
        a = ggplot()
    }

    #FORMAT axis                                                                            
    a=a+scale_x_continuous("Longitude",limits = bbox[c('left', 'right')],
                           expand = c(0, 0))               
    a=a+ scale_y_continuous("Latitude",limits = bbox[c('bottom', 'top')], 
                            expand = c(0, 0))                
                                                                                        
    a=a+theme(axis.text.x=element_text(size=25),axis.title.x=element_text(size=25))         
    a=a+theme(axis.text.y=element_text(size=25),axis.title.y=element_text(size=25))         

    require(maps)
    m = map_data("world") %>% filter(region!='Antarctica')
    m$long[m$long< -30] <- m$long[m$long< -30] +360   
    lower_boundary <- m$lat < -38
    m$lat[m$lat< -38] <- -38

    if(fancy_proj){
        a = a + geom_polygon(data=m, aes(x=long, y=lat, group=group),  color='black',
                             fill='#dddddd')
        a = add_eems_overlay(a, mcmcpath, is.mrates)
        a = a + geom_path(data=m, aes(x=long, y=lat, group=group),  color='black')
        a = a + coord_map("mollweide",orientation=c(90,10, 40)) 
        a = a + xlim(-20, 195) + ylim(-40, 80)
        a= a+ scale_y_continuous("Latitude",limits = c(-40, 80), 
                                expand = c(0, 0))                
        a = a + theme_classic()
        #a = a + coord_map("mollweide",orientation=c(90,40, 110)) #worldmap
    } else {
        a = add_eems_overlay(a, mcmcpath, is.mrates)
        a = a + geom_path(data=m, aes(x=long, y=lat, group=group),  color='black')
    }

    #a=a+gg_add_samples(mcmcpath)
    #a=a+scale_size_identity(guide="none")
    #a=a+coord_fixed()

    #ggsave(sprintf("test%s.png", RES), a, width=11, height=8)
    return(a)
}

gg_add_samples_true <- function(map, popgeo, popdisplay){
    pm <- load_pop_meta(popgeo, popdisplay)
    pm <- pm[!is.na(pm$longitude),]                                          
    pm$longitude[pm$longitude < -30] <- pm$longitude[pm$longitude< -30]+360  

    map + geom_text(data=pm, aes(label=abbrev, x=longitude, y=latitude)) 
}


ggscatterplot <- function(mcmcpath,pop_display_file, indiv_label_file, 
                          remove.singletons=F, outlier_file, ...) {
    print('Plotting average dissimilarities within and between demes')
    mcmcpath1 <- character()
    for (path in mcmcpath) {
        if (file.exists(paste(path,'/rdistJtDobsJ.txt',sep=''))&&
            file.exists(paste(path,'/rdistJtDhatJ.txt',sep=''))&&
            file.exists(paste(path,'/rdistoDemes.txt',sep=''))) {
            mcmcpath1 <- c(mcmcpath1,path)
        }
    }
    mcmcpath <- mcmcpath1
    nsimnos <- length(mcmcpath)
    if (nsimnos==0) { return(0) }
    ## List of observed demes, with number of samples taken collected
    ## Each row specifies: x coordinate, y coordinate, n samples
    oDemes <- scan(paste(mcmcpath[1],'/rdistoDemes.txt',sep=''),quiet=TRUE)
    oDemes <- matrix(oDemes,ncol=3,byrow=TRUE)
    Sizes <- oDemes[,3]
    nPops <- length(Sizes)
    matSize <- matrix(Sizes,nPops,nPops)
    minSize <- pmin(matSize,t(matSize))

    JtDobsJ <- matrix(0,nPops,nPops)
    JtDhatJ <- matrix(0,nPops,nPops)
    for (path in mcmcpath) {
        print(path)
        JtDobsJ <- JtDobsJ + as.matrix(read.table(paste(path,'/rdistJtDobsJ.txt',sep=''),header=FALSE))
        JtDhatJ <- JtDhatJ + as.matrix(read.table(paste(path,'/rdistJtDhatJ.txt',sep=''),header=FALSE))
    }
    JtDobsJ <- JtDobsJ/nsimnos
    JtDobsJ[is.nan(JtDobsJ)] <- median(diag(JtDobsJ), na.rm=T) #nan fix
    JtDhatJ <- JtDhatJ/nsimnos

    pop_labels <- get_fit_matrix_abbrev(mcmcpath, indiv_label_file, pop_display_file)
    pop_ids <- get_fit_matrix_ids(mcmcpath, indiv_label_file, pop_display_file)
    pop_labels_full<- get_fit_matrix_full(mcmcpath, indiv_label_file, pop_display_file)
    label_mat <- outer(FUN=paste, pop_labels, pop_labels, sep="-")
    #label_mat <<- label_mat


    if (remove.singletons) {
        print("REMOVING SINGLETONS")
        remove <- which(Sizes<=1)
        if (length(remove)) {
            JtDobsJ <- JtDobsJ[-remove,-remove]
            JtDhatJ <- JtDhatJ[-remove,-remove]
            minSize <- minSize[-remove,-remove]
            Sizes <- Sizes[-remove]
            nPops <- length(Sizes)
            label_mat <- label_mat[-remove, -remove]
            pop_labels <- pop_labels[-remove]
            pop_labels_full <- pop_labels_full[-remove]
            pop_ids <- pop_ids[-remove]
        }
    }
    if (nPops<2) {
        print('Need at least two observed demes to plot pairwise differences')
        return (0)
    }

    dmat <- as.matrix(rdist.earth(oDemes[,1:2]))
    dpts <- dmat[upper.tri(dmat, diag=FALSE)]

    Wobs <- diag(JtDobsJ)
    What <- diag(JtDhatJ)
    ones <- matrix(1,nPops,1)
    Bobs <- JtDobsJ - (Wobs%*%t(ones) + ones%*%t(Wobs))/2
    Bhat <- JtDhatJ - (What%*%t(ones) + ones%*%t(What))/2
    xpts <- Bhat[upper.tri(Bhat,diag=FALSE)]
    ypts <- Bobs[upper.tri(Bobs,diag=FALSE)]
    label_mat <- label_mat[upper.tri(label_mat, diag=FALSE)]
    

    cnts <- minSize[upper.tri(minSize,diag=FALSE)]
    par(font.main=1)
    plot(xpts,ypts,col=NULL,
         xlab=expression(paste("Fitted dissimilarity between demes,  ",hat(D)[ab]," - (",hat(D)[aa],"+",hat(D)[bb],")/2",sep="")),
         ylab=expression(paste("Observed dissimilarity between demes,  ",D[ab]," - (",D[aa],"+",D[bb],")/2",sep="")), ...)
    if (remove.singletons) {
        title(main="Dissimilarities between demes\nSingleton demes, if any, excluded from plot (not from EEMS)")
    } else {
        title(main="Dissimilarities between demes\nGray means a or b has a single individual sampled from")
    }
    text(xpts,ypts,col=c("black","gray60")[1+1*(cnts==1)], labels=label_mat)
    abline(0, 1, col='grey', lty=2)


    ypts <- Wobs
    xpts <- What
    plot(xpts,ypts,col=NULL,
         xlab=expression(paste("Fitted dissimilarity within demes,  ",hat(D)[aa],sep="")),
         ylab=expression(paste("Observed dissimilarity within demes,  ",D[aa],sep="")), ...)
    if (remove.singletons) {
        title(main="Dissimilarities within demes\nSingleton demes, if any, excluded from plot (not from EEMS)")
    } else {
        title(main="Dissimilarities within demes\nGray means a has a single individual sampled from")
    }
    text(xpts,ypts,col=c("black","gray60")[1+1*(cnts==1)], labels=pop_labels)
    abline(0, 1, col='grey', lty=2)





    diag(Bobs) <- 0
    diag(Bhat) <- 0
    print(dim(Bobs))
    Bobs <<- Bobs
    Bhat <<- Bhat
    print(dim(Bobs))
    abs_error <- abs(Bobs-Bhat)                  
    m <- apply(abs_error, 2, median)             
    mad <- apply( abs(abs_error -m), 2, median)  

    error_by_pop <- mad / median(mad)
    o20 <- order(error_by_pop, decreasing=T)[1:min(20, length(error_by_pop))]
    barplot(error_by_pop[o20], names.arg=pop_labels_full[o20], las=2, cex.names=0.6)
    title("Median Abs Error of Fitted Dissimilarities (Top 20)")
    o <- order(error_by_pop, decreasing=T)
    barplot(error_by_pop[o], names.arg=pop_labels_full[o], las=2, cex.names=0.6
            )
    title("Median Abs Error of Fitted Dissimilarities")

    write.table(data.frame(popId=pop_ids, error=error_by_pop), outlier_file, 
                row.names=F)

    #=------------------- stuff w
    dmat <- rdist.earth(oDems[,1:2])
    xpts <- dmat[upper.tri(dmat, diag=FALSE)]
    ypts <- Bobs[upper.tri(Bobs,diag=FALSE)]
    plot(xpts,ypts,col=NULL,
         xlab=expression(paste("Geographic distance between demes,  ",hat(D)[aa],sep="")),
         ylab=expression(paste("Observed genetic dissimilarity between demes,  ",D[aa], " (km)", sep="")), ...)
    text(xpts,ypts,col=c("black","gray60")[1+1*(cnts==1)], labels=pop_labels)
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

ggadd.graph <- function(g){
    xstart <- g$demes[g$edges[,1],1]
    xend <- g$demes[g$edges[,2],1]
    ystart <- g$demes[g$edges[,1],2]
    yend <- g$demes[g$edges[,2],2]
    grid <- data.frame(xstart, xend, ystart, yend)
    geom_segment(aes(x=xstart, y=ystart, xend=xend, yend=yend), data=grid, color='#eeeeee80')
}
ggadd.pts <- function(g){
    tbl <- table(g$ipmap)
    ind <- as.numeric(names(tbl))
    sizes <- as.vector(tbl)
    df <- data.frame(x=g$demes[ind,1], y=g$demes[ind,2], sizes=sizes)
    geom_point(aes(x=x, y=y, size=sizes), data=df)
                      
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

