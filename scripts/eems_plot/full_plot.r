source("scripts/eems_plot/contour.r")
source("scripts/eems_plot/compute_spatial_posterior.r")
source("scripts/eems_plot/plot_raw_data.r")
source("scripts/eems_plot/load_output.r")

DIMNS=100

cv <- sapply(c(rep(0,30),0:70/70), function(a) adjustcolor('white', a))  
cv <- sapply(c(0:100/100), function(a) adjustcolor('white', a))  

plot_q <- function(mcmcpath, mode='logmean', standardize=F){
    dimns <- read.dimns(mcmcpath[1], DIMNS, DIMNS)
    qfiles <- read.qfiles(mcmcpath)
    #sigmas <- sapply(mcmcpath, function(x)
    #		     read.table(sprintf("%s/mcmcthetas.txt", x))[,1])
    #s2 <- c(sigmas) 
    #s2 <- s2[s2>0]

    pts <- compute.pts(dimns, qfiles, standardize=standardize)
    #pts <- pts * s2 / mean(s2)

    plot.eems.contour(dimns, pts, col=default.eems.colors('q'),
		      mode=mode)
		      #mode=mode, col.range=c(0.9,1.1))
    
    #plot.eems.contour.var(dimns, pts, col=cv)
    add.map()
    add.grid(mcmcpath)
    add.samples(mcmcpath, cex.samples=1.3)
    pts <<- pts
    dimns <<- dimns
}
plot_q_summary <- function(mcmcpath,
                           pop_geo_file,
                           pop_display_file,
                           mode='logmean', standardize=F){
    print(mcmcpath)
    dimns <- read.dimns(mcmcpath[1], DIMNS, DIMNS)
    qfiles <- read.qfiles(mcmcpath)
    pts <- compute.pts(dimns, qfiles, standardize=standardize)
     
    plot.eems.contour(dimns, pts, col=default.eems.colors('q'),
		      mode=mode)
    
    #plot.eems.contour.var(dimns, pts, col=cv)
    add.map(border='grey')
    #add.grid(mcmcpath)
    pg <- add.samples_true(pop_geo_file, 
                     pop_display_file,
                     cex=1.3)
}

plot_m <- function(mcmcpath, mode='logmean', standardize=F){
    print(mcmcpath)
    dimns <- read.dimns(mcmcpath[1], DIMNS, DIMNS)
    mfiles <- read.mfiles(mcmcpath)
    pts <- compute.pts(dimns, mfiles, standardize=standardize)
    if(standardize){
	col.range=NULL
    } else {
	col.range=c(0.1, 1000)
    }
    plot.eems.contour(dimns, pts, col=default.eems.colors('m'),
		      mode=mode, col.range=col.range)
    
    #plot.eems.contour.var(dimns, pts, col=cv)
    add.map()
    add.grid(mcmcpath)
    add.samples(mcmcpath, cex.samples=1.3)
}

plot_m_summary <- function(mcmcpath,
                           pop_geo_file,
                           pop_display_file,
                           mode='logmean', standardize=F){
    print(mcmcpath)
    dimns <- read.dimns(mcmcpath[1], DIMNS, DIMNS)
    mfiles <- read.mfiles(mcmcpath)
    pts <- compute.pts(dimns, mfiles, standardize=standardize)
    if(standardize){
	col.range=NULL
    } else {
	col.range=c(0.1, 1000)
    }
    plot.eems.contour(dimns, pts, col=default.eems.colors('m'),
		      mode=mode, col.range=col.range)
    
    #plot.eems.contour.var(dimns, pts, col=cv)
    add.map(border='grey')
    #add.grid(mcmcpath)
    pg <- add.samples_true(pop_geo_file, 
                     pop_display_file,
                     cex=1.3)
}

plot_map <- function(mcmcpath){
    dimns <- read.dimns(mcmcpath[1], DIMNS, DIMNS)
    par(mar=c(0,0,0,0))
    plot(NA, xlim=dimns$xrange, ylim=dimns$yrange,axes=F, xlab='', ylab='',
	 xaxs='i', yaxs='i', asp=1) 
    add.map(col='lightgray')
    add.grid(mcmcpath, col='#ffb000')
    add.samples(mcmcpath, cex.samples=1.9)
}

plot_m_all <- function(mcmcpath, ...){
    plot_m(mcmcpath, 'logmean', T)
    plot_m(mcmcpath, 'logmean', F)
    plot_m(mcmcpath, 'mean', F)
    plot_m(mcmcpath, 'median', F)
}

plot_q_all <- function(mcmcpath, ...){
    plot_q(mcmcpath, 'logmean', T)
    plot_q(mcmcpath, 'logmean', F)
    plot_q(mcmcpath, 'mean', F)
    plot_q(mcmcpath, 'median', F)
}

plot.to.pdf <- function( plotf, plotpath='tmp', dimns=NULL, ...){
    ext=''
    if( identical(plotf, plot_q) || identical(plotf, plot_q_all) ){
	ext = '-qrates'
	plot.height <- 11
	ratio = dimns$yspan / dimns$xspan
	plot.width <- plot.height / ratio 
    } else if( identical(plotf, plot_m) || identical(plotf, plot_m_all)){
	ext = '-mrates'
	plot.height <- 11
	ratio = dimns$yspan / dimns$xspan
	plot.width <- plot.height / ratio 
    } else if( identical(plotf, plot_m_summary) ){
	ext = '-mrates'
	plot.height <- 11
	ratio = dimns$yspan / dimns$xspan
	plot.width <- plot.height / ratio 
    } else if( identical(plotf, plot_q_summary) ){
	ext = '-qrates'
	plot.height <- 11
	ratio = dimns$yspan / dimns$xspan
	plot.width <- plot.height / ratio 
    } else if( identical(plotf, plot_map)){
	ext = '-map'
	plot.height <- 11
	ratio = dimns$yspan / dimns$xspan
	plot.width <- plot.height / ratio 
    } else if( identical(plotf, plot_raw) ){
	ext = '-raw'
	plot.height <-8 
	plot.width <- 11.5
    } else if( identical(plotf, plot.traces) ){
	ext = '-traces'
	plot.height=8
	plot.width=11.5
    } else if( identical(plotf, dist.scatterplot) ){
	ext = '-scatter'
	plot.height=16
	plot.width=16
    }
    save.graphics(paste0(plotpath,ext), plot.height, plot.width, out.png=T)
    plotf(...)
    dev.off()
}

plot.to.png <- function( plotf, plotpath='tmp', dimns=NULL, ...){
    ext=''
    if( identical(plotf, plot_q) || identical(plotf, plot_q_all) ){
	ext = '-qrates'
	plot.height <- 500
	ratio = dimns$yspan / dimns$xspan
	plot.width <- plot.height / ratio 
    } else if( identical(plotf, plot_m) || identical(plotf, plot_m_all)){
	ext = '-mrates'
	plot.height <- 500 
	ratio = dimns$yspan / dimns$xspan
	plot.width <- plot.height / ratio 
    } else if( identical(plotf, plot_raw) ){
	ext = '-raw'
	plot.height <-500
	plot.width <- 800
    } else if( identical(plotf, plot.traces) ){
	ext = '-traces'
	plot.height=500
	plot.width=800
    } else if( identical(plotf, dist.scatterplot) ){
	ext = '-scatter'
	plot.height=400
	plot.width=400
    }
    save.graphics(paste0(plotpath,ext), plot.height, plot.width, out.png=T)
    plotf(...)
    dev.off()
}
save.graphics <- function(plotpath,plot.height,plot.width,res=300,
			  out.png=FALSE, ...) {
    if (out.png) {
        bitmap(paste(plotpath,'%02d.png',sep=''),type='png16m',res=res,
               height=plot.height,width=plot.width,units='in')
    } else {
        pdf(paste(plotpath,'%02d.pdf',sep=''),
              height=plot.height,width=plot.width,onefile=FALSE)
    }
}

plot.logposterior <- function(mcmcpath) {
    print('Plotting log posterior')
    mcmcpath1 <- character()
    for (path in mcmcpath) {
        if (file.exists(paste(path,'/mcmcpilogl.txt',sep=''))) {
            mcmcpath1 <- c(mcmcpath1,path)
        }
    }
    mcmcpath <- mcmcpath1
    nsimnos <- length(mcmcpath)
    colors <- rep_len(1:8,length.out=nsimnos)
    ltypes <- rep_len(1:3,length.out=nsimnos)
    if (nsimnos==0) { return(0) }
    posteriors <- list()
    yrange <- NULL
    niter <- NULL
    for (i in 1:length(mcmcpath)) {
        path <- mcmcpath[i]; print(path)
        pilogl <- scan(paste(path,'/mcmcpilogl.txt',sep=''),quiet=TRUE)
        pilogl <- matrix(pilogl,ncol=2,byrow=TRUE)
        posterior <- pilogl[,1] + pilogl[,2]
	posterior <- posterior[!posterior == 0]
        posteriors[[i]] <- posterior
        yrange <- range(c(yrange,posterior))
        niter <- max(niter,length(posterior))
    }
    plot(c(1,niter),yrange,type="n",xlab="iteration (after thinning)",ylab="log posterior")
    for (i in 1:length(mcmcpath)) {
        posterior <- posteriors[[i]]
        lines(1:length(posterior),posterior,col=colors[i],lty=ltypes[i],lwd=2)
    }
    legend("bottomleft",legend=1:nsimnos,col=colors[1:nsimnos],lty=ltypes[1:nsimnos],lwd=2,bty="n",horiz=TRUE)
}

plot.traces <- function(mcmcpath) {
    files <- c('/mcmcpilogl.txt', '/mcmcmhyper.txt', '/mcmcmtiles.txt',
	       '/mcmcthetas.txt', '/mcmcqhyper.txt', '/mcmcqtiles.txt',
	       '/mcmcmrates.txt', '/mcmcqrates.txt')
    mcmcpath <- check.files.at.path(files, mcmcpath)

    par(mfrow=c(5,2), mar=c(4,4,.1,.1))
    plot.posterior(mcmcpath)
    plot.param(mcmcpath, files[2], 1, ylab='m[1]')
    plot.param(mcmcpath, files[2], 2, ylab='m[2]')
    plot.param(mcmcpath, files[3], 1, ylab='# mtiles')
    plot.param(mcmcpath, files[5], 2, ylab='q[2]')
    plot.param(mcmcpath, files[6], 1, ylab='# qtiles')
    plot.param(mcmcpath, files[4], 1, ylab='sigma2')
    plot.param(mcmcpath, files[4], 2, ylab='df')
    plot.mean.rates(mcmcpath, files[7], 2, ylab='Mean(mrates)')
    plot.mean.rates(mcmcpath, files[8], 2, ylab='Mean(qrates)')
}

plot.posterior <- function(mcmcpath){
    data <- sapply(mcmcpath, function(x){
		   d <- rowSums(read.table(paste0(x, '/mcmcpilogl.txt')))
		   })
    data[data==0] <- NA
    xlim <- c(0, max(which(!is.na(data), arr.ind=T)))
    ylim <- range(data, na.rm=T)
    plot(NA, xlim=xlim, ylim=ylim, ylab='log posterior',
	 xlab='iteration (after thinning)')
    for(i in 1:ncol(data)){
	lines(data[,i], col=i)
    }
}

plot.mean.rates <- function(mcmcpath, fname, ...){
    data <- lapply(mcmcpath, function(x){
		   rowMeans((read.table(paste0(x, fname), fill=T,
				   row.names=NULL, header=F)),
			   na.rm=T)})
    xlim <- c(0, max(sapply(data, length)))
    ylim <- range(data, na.rm=T)
    plot(NA, xlim=xlim, ylim=ylim, 
	 xlab='iteration (after thinning)', ...)
    for(i in 1:length(data)){
	lines(data[[i]], col=i)
    }
    return(data)
}

plot.param <- function(mcmcpath, fname, column=1, ...){
    data <- sapply(mcmcpath, function(x)
		   d <- read.table(paste0(x, fname))[,column]
		   )
    data[data==0] <- NA
    xlim <- c(0, max(which(!is.na(data), arr.ind=T)))
    ylim <- range(data, na.rm=T)
    print(xlim)
    print(ylim)
    plot(NA, xlim=xlim, ylim=ylim, 
	 xlab='iteration (after thinning)', ...)
    for(i in 1:ncol(data)){
	lines(data[,i], col=i)
    }
    data
}

dist.scatterplot <- function(mcmcpath,pop_display_file, indiv_label_file, remove.singletons=TRUE, ...) {
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
    if (nsimnos>1) {
        for (simno in 2:nsimnos) {
            path <- mcmcpath[simno]; print(path)
            oDemes2 <- scan(paste(path,'/rdistoDemes.txt',sep=''),quiet=TRUE)
            oDemes2 <- matrix(oDemes2,ncol=3,byrow=TRUE)
            #if ((length(oDemes2)!=length(oDemes))||
            #    (sum(oDemes2!=oDemes)>0)) {
            #    stop('dist.scatterplot: mcmc results for different population graphs.')
            #}
        }
    }
    Sizes <- oDemes[,3]
    nPops <- length(Sizes)
    matSize <- matrix(Sizes,nPops,nPops)
    minSize <- pmin(matSize,t(matSize))
    JtDobsJ <- matrix(0,nPops,nPops)
    JtDhatJ <- matrix(0,nPops,nPops)
    for (path in mcmcpath[1]) {
        print(path)
        JtDobsJ <- JtDobsJ + as.matrix(read.table(paste(path,'/rdistJtDobsJ.txt',sep=''),header=FALSE))
        JtDhatJ <- JtDhatJ + as.matrix(read.table(paste(path,'/rdistJtDhatJ.txt',sep=''),header=FALSE))
    }
    JtDobsJ <- JtDobsJ/nsimnos
    JtDhatJ <- JtDhatJ/nsimnos
    if (remove.singletons) {
        remove <- which(Sizes<=1)
        if (length(remove)) {
            JtDobsJ <- JtDobsJ[-remove,-remove]
            JtDhatJ <- JtDhatJ[-remove,-remove]
            minSize <- minSize[-remove,-remove]
            Sizes <- Sizes[-remove]
            nPops <- length(Sizes)
        }
    }
    if (nPops<2) {
        print('Need at least two observed demes to plot pairwise differences')
        return (0)
    }

    g <- read.output.graph(mcmcpath[1])
    d.obs <- g$demes[unique(g$ipmap),]
    require(fields)
    dmat <- rdist.earth(d.obs)
    dpts <- dmat[upper.tri(dmat, diag=FALSE)]

    Wobs <- diag(JtDobsJ)
    What <- diag(JtDhatJ)
    ones <- matrix(1,nPops,1)
    Bobs <- JtDobsJ - (Wobs%*%t(ones) + ones%*%t(Wobs))/2
    Bhat <- JtDhatJ - (What%*%t(ones) + ones%*%t(What))/2
    ypts <- Bobs[upper.tri(Bobs,diag=FALSE)]
    xpts <- Bhat[upper.tri(Bhat,diag=FALSE)]
    
    print("FIT MAT DEBUG")
    print(mcmcpath)
    print(indiv_label_file)
    print(pop_display_file)
    labels <- get_fit_matrix(mcmcpath, indiv_label_file, pop_display_file)
    labels <- labels[upper.tri(labels, diag=FALSE)]

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
    text(xpts,ypts,col=c("black","gray60")[1+1*(cnts==1)], labels=labels)


    ypts <- Wobs
    xpts <- What
    plot(xpts,ypts,col=c("black","gray60")[1+1*(Sizes==1)],
         xlab=expression(paste("Fitted dissimilarity within demes,  ",hat(D)[aa],sep="")),
         ylab=expression(paste("Observed dissimilarity within demes,  ",D[aa],sep="")), ...)
    if (remove.singletons) {
        title(main="Dissimilarities within demes\nSingleton demes, if any, excluded from plot (not from EEMS)")
    } else {
        title(main="Dissimilarities within demes\nGray means a has a single individual sampled from")
    }
}

plot.all.posterior.stuff <- function(pop_display, pop_geo, indiv_label,
                                     mcmcpath, datapath=datapath, 
				     plotpath='tmp', grid=100){
    mcmcpath <- check.files.at.path('/mcmcqhyper.txt', mcmcpath)
    dimns <- read.dimns(mcmcpath[1], DIMNS, DIMNS)
#    plot_pca_talk(datapath, plotpath)

    plot.to.pdf(plot_map, plotpath=plotpath, mcmcpath=mcmcpath, 
		dimns=dimns)
    plot.to.pdf(plot_m_summary, plotpath=plotpath, mcmcpath=mcmcpath, 
		mode='logmean', dimns=dimns, pop_geo_file=pop_geo,
                pop_display_file=pop_display)
    print('1')
    plot.to.pdf(plot_q_summary, plotpath=plotpath, mcmcpath=mcmcpath, 
		mode='logmean', dimns=dimns, pop_geo_file=pop_geo,
                pop_display_file=pop_display)
    print('2')
    plot.to.pdf(plot.traces, plotpath=plotpath, mcmcpath=mcmcpath)
    print('3')
    plot.to.pdf(dist.scatterplot, plotpath=plotpath, mcmcpath=mcmcpath,
                pop_display_file=pop_display,
                indiv_label_file=indiv_label,
		remove.singletons=F)
    print('4')
}

add.samples_true <- function(pop_geo_file, 
                             pop_display_file,
                             pch=16, ...){
    source("scripts/load_pop_meta.R")
    pm <- load_pop_meta(pop_geo_file, pop_display_file)
    text(pm$longitude, pm$latitude, pm$abbrev, pch=pch, ...)
    pm
}


get_fit_matrix <- function(mcmcpath, indiv_label, pop_display){
    pop_display <- read.csv(pop_display)
    o <- read.table(sprintf("%s/ipmap.txt", mcmcpath[1]))
    names(o) <- 'grid'
    o <- cbind(grid=o, grid_order=1:nrow(o))
    indiv_label <- read.csv(indiv_label)     
    i2 <- bind_cols(indiv_label,grid=o) %>% left_join(pop_display)
    x <- i2 %>% group_by(grid) %>% 
        summarize(grid_order=first(grid_order), f=first(abbrev)) %>% 
        arrange(grid_order) %>% select(f) %>% mutate(f=as.character(f))
    return(outer(FUN=paste, x$f, x$f, sep="-"))
}
