#library(rgdal)
#library(jpeg)
suppressPackageStartupMessages({
require(maps)
require(ggrepel)
source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/eems.plots.ggplot.R")
})


#RES=20

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
}

get_boundary_map <- function(){
    print("iterior only")
    regions <- map(namesonly=T, plot=F)
    regions <- regions[substr(regions, 1, 5) != 'Antar']
    m1 <- map(regions=regions, interior=F, plot=F, xlim=c(-30, 180), resolution=0)
    m2 <- map(regions=regions, interior=F, plot=F, xlim=c(-180, -30), resolution=0)
    m1$group <- cumsum(is.na(m1$x))
    m2$group <- cumsum(is.na(m2$x)) + max(m1$group) + 1
    m <- data.frame(long=c(m1$x,m2$x), lat=c(m1$y, m2$y),
		    group=c(m1$group, m2$group))
    m <- m %>% filter(!is.na(long))
    m$long[m$long< -30] <- m$long[m$long< -30] +360   
    m$lat[m$lat< -38] <- -38
    print("got interior")
    return(m)
}
get_fill_map <- function(){
    m = map_data("world") %>% filter(region!='Antarctica')
    m$long[m$long< -30] <- m$long[m$long< -30] +360   
    lower_boundary <- m$lat < -38
    m$lat[m$lat< -38] <- -38
    m
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


make_map <- function(mcmcpath, zoom=6, is.mrates=T, fancy_proj=F, just_map=F, interior=F,
		     fancy_proj_pars=c(90,10, 40), ...){
    boundary <- read.table(sprintf("%s/outer.txt", mcmcpath[1]))
    bbox <- c(left=min(boundary[1]), right=max(boundary[1]),
              bottom=min(boundary[2]), top=max(boundary[2]))
    bbox['top'] <- pmin(bbox['top'], 85)

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

    xlim_map <- c(bbox[c('left', 'right')])
    ylim_map <- c(bbox[c('bottom', 'top')])

    #FORMAT axis                                                                            
    a=a+scale_x_continuous("Longitude")
    a=a+ scale_y_continuous("Latitude")
                                                                                        
    a=a+theme(axis.text.x=element_text(size=12),axis.title.x=element_text(size=12))         
    a=a+theme(axis.text.y=element_text(size=12),axis.title.y=element_text(size=12))         

    m_fill <- get_fill_map()
    m_boundary <- get_boundary_map()

    if(fancy_proj){
	if(interior){
	    a = a + geom_polygon(data=m_fill, aes(x=long, y=lat, group=group),  color='#dedede',
				 fill='#aaaaaa')
	} else {
	    a = a + geom_polygon(data=m_fill, aes(x=long, y=lat, group=group),  color='#aaaaaa',
				 fill='#aaaaaa')
	}
    	if(!just_map){
	    a = add_eems_overlay(a, mcmcpath, is.mrates, ...)
	}
	if(interior){
	    a = a + geom_path(data=m_fill, aes(x=long, y=lat, group=group),  color='#dedededd')
	}else {
	    a = a + geom_path(data=m_boundary, aes(x=long, y=lat, group=group),  color='#222222dd')
	}
        a = a + coord_map("mollweide",orientation=fancy_proj_pars,
			  xlim=xlim_map, ylim=ylim_map) + xlim(-24, 193)+ ylim(-40, 78)

	print("plotting fancy")
        a = a + theme_classic() #+ theme(panel.background = element_rect(colour = "#efefef")
        #a = a + coord_map("mollweide",orientation=c(90,40, 110)) #worldmap
    } else {
	a=a+coord_map("mercator", parameters=NULL,  xlim=bbox[c('left', 'right')],
			   ylim=bbox[c('bottom', 'top')]
			   ) + 
	    xlim(bbox[c('left', 'right')])+ 
	    ylim(bbox[c('bottom', 'top')])
    	if(!just_map){
	    a = add_eems_overlay(a, mcmcpath, is.mrates,...)
	}
	if(interior){
	    a = a + geom_path(data=m_fill, aes(x=long, y=lat, group=group),  color='#222222dd')
	}else {
	    a = a + geom_path(data=m_boundary, aes(x=long, y=lat, group=group),  color='#222222dd')
	}
	print("done with map fn")
    }

    #a=a+gg_add_samples(mcmcpath)
    #a=a+scale_size_identity(guide="none")
    #a=a+coord_fixed()

    #ggsave(sprintf("test%s.png", RES), a, width=11, height=8)
    return(a)
}


gg_add_samples_true <- function(map, popgeo, popdisplay, size=4){
    pm <- load_pop_meta(popgeo, popdisplay)
    pm <- pm[!is.na(pm$longitude),]                                          
    pm$longitude[pm$longitude < -30] <- pm$longitude[pm$longitude< -30]+360  

    map <- .gg_add_samples_true(map, pm, type="pointscol")
    map <- .gg_add_samples_true(map, pm, size=size)

}

.gg_add_samples_true <- function(map, pm, type="label_repel", size=4, ...){
    if(type== "label_repel"){
	m <- map + geom_label_repel(data=pm, aes(label=abbrev, x=longitude, y=latitude),
		    color='#222222dd', size=size, fill="#ffffff10",
			family="Arial",
			label.padding = unit(1e-9, "lines"),
			label.r=unit(1e-9, "lines"),
			label.size=0,
			min.segment.length=unit(0,"lines"),
			segment.size=0.2,
			box.padding = unit(0.01, "lines"),
			force=.5,
			max.iter=10000,
		           segment.color = 'black', ...)	
	return(m)

    }
    if(type=="points"){
	m <- map + geom_point(data=pm, aes(x=longitude, y=latitude), ...)
    }
    #hack for poster
    if(type=="pointscol"){
    source("scripts/assign_color_by_coord.R")
    pm$color <- get_cols_wrap(pm)
	m <- map + geom_point(data=pm, aes(x=longitude, y=latitude, color=color),
                          size=1,
                          ...) +
		    scale_color_identity()+
            scale_size_identity()
    }

    m
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

ggadd.graph <- function(g, color="#eeeeee50"){
    xstart <- g$demes[g$edges[,1],1]
    xend <- g$demes[g$edges[,2],1]
    ystart <- g$demes[g$edges[,1],2]
    yend <- g$demes[g$edges[,2],2]
    grid <- data.frame(xstart, xend, ystart, yend)
    geom_segment(aes(x=xstart, y=ystart, xend=xend, yend=yend), data=grid, color=color)
}
ggadd.pts <- function(g, color="#efefefdd", const_size=T){
    tbl <- table(g$ipmap)
    ind <- as.numeric(names(tbl))
    sizes <- as.vector(tbl)
    df <- data.frame(x=g$demes[ind,1], y=g$demes[ind,2], sizes=sizes)
    if(const_size) {
	pts <- geom_point(aes(x=x, y=y), data=df, color=color, size=1.8)
    } else {
	pts <- geom_point(aes(x=x, y=y, size=sizes), data=df, color=color)
    }
}

ggadd.pts.color <- function(g, const_size=T){
    tbl <- table(g$ipmap)
    ind <- as.numeric(names(tbl))
    sizes <- as.vector(tbl)

    df <- data.frame(longitude=g$demes[ind,1], latitude=g$demes[ind,2], sizes=sizes)

    source("scripts/assign_color_by_coord.R")
    df$color <- get_cols_wrap(df)
    df$x <- df$longitude
    df$y <- df$latitude


    if(const_size) {
	pts <- list(geom_point(aes(x=x, y=y, color=color), data=df, size=2.0) , 
		    scale_color_identity(),
            scale_size_identity(),
		    geom_point(aes(x=x, y=y), size=2.0, data=df, color="black", pch=1, stroke=.2))
    } else {
	pts <- list(geom_point(aes(x=x, y=y, size=sizes, color=color), data=df) , scale_color_identity())
    }
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

