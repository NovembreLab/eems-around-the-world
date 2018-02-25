suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(fields)
    library(scales)
    source("scripts/load_pop_meta.R")
})

COL_GREY <- 'grey'

# function that colors stuff accoring to annotation in config files
annotate <- function(error){
    cfg <- snakemake@config
    error$color <- COL_GREY
    for(a in CC$annotations){
	filter <- cfg$annotation[[a]]$from_filter
	popId <- cfg$filter[[filter]]
	new_col <- cfg$plot$annotations[[a]]$color
	print(paste("annotate:", filter, a, new_col))
	if("popId" %in% names(error)){
	    b <- error$popId  %in% popId
	    error$color[b] <- new_col
	}
    }
    error
}

get_worst_errors <- function(dist, pop_display){
    d0 <- dist %>% left_join(pop_display, by=c(popId.x="popId")) %>%   
	left_join(pop_display, by=c(popId.y="popId")) %>%
	mutate(label=sprintf("%s|%s", abbrev.x, abbrev.y)) %>%
	mutate(err=abs(eemsdist-gendist)/(gendist+0.001))  %>%
	mutate(color=color_mean(color.x, color.y)) %>%
	arrange(-err)
}
get_worst_errors_ind <- function(inddist, im, pd){
    im <- left_join(im ,pd) %>% select(sampleId, color, abbrev)
    d0 <- inddist %>% filter(geoDist>1e-2)  %>%
	select( -starts_with("pc"))  %>%    
        left_join(im, by=c(sampleId.x="sampleId")) %>%            
        left_join(im, by=c(sampleId.y="sampleId")) %>%
	mutate(color=color_mean(color.x, color.y)) %>%
	mutate(label=sprintf("%s|%s", abbrev.x, abbrev.y)) %>%
	mutate(err=abs(eemsdist-gendist)/(gendist+0.001))  %>%
	arrange(-err)
}
get_marginal_ind <- function(dist, im, pd){
    im <- left_join(im ,pd) %>% select(sampleId, color, abbrev)
    dist <- dist %>% select(-geoDist, -starts_with("pc"))  %>%
	left_join(im, by=c(sampleId.x="sampleId")) %>% 
	left_join(im, by=c(sampleId.y="sampleId"))
    d2 <- dist %>% 
	mutate(tmp= sampleId.y, sampleId.y = sampleId.x, sampleId.x=tmp) %>% 
        select(-tmp) %>% bind_rows(dist) %>%
        mutate(err=abs(eemsdist-gendist)/(gendist+0.001)) %>%
            group_by(sampleId.x) %>% summarize(err=median(err), 
					       color=first(color.y),
					       pop=first(abbrev.y)) %>%
            arrange(-err) %>% dplyr::rename(sampleId=sampleId.x) %>%
            arrange(-err)
    d2
}

get_marginal <- function(dist, pop_display){
    d2 <- dist %>% mutate(tmp = popId.y, popId.y = popId.x, popId.x=tmp) %>% 
        select(-tmp) %>% bind_rows(dist) %>%
        mutate(err=abs(eemsdist-gendist)/(gendist+0.001)) %>%
            group_by(popId.x) %>% summarize(err=median(err)) %>%
            arrange(-err) %>% dplyr::rename(popId=popId.x) %>%
            left_join(pop_display) %>%
            ungroup %>% arrange(-err)
}


first_non_grey <- function(x){
    n <- n_distinct(x)
    if(n==1) return(x[1])
    x <- x[x!=COL_GREY]
    n <- n_distinct(x)
    if(n==1) return(x[1])
    return("red")
}

v_non_grey <- function(x,y){
    o <- x
    cond1 <- x!=y & x==COL_GREY
    o[cond1]  <- y[cond1]
#    cond2 <- x!=y & y==COL_GREY
#    o[cond2]  <- x[cond2]
    cond3 <- x!=y & x!=COL_GREY & y!=COL_GREY
    o[cond3]  <- "red"
    o

}

color_mean <- function(x, y){
    require(abind)
    abind(col2rgb(x) , col2rgb(y), along=3) %>% 
    apply(c(1,2), mean) %>% t %>% rgb(maxColorValue=256)
}

get_marginal_grid <- function(dist, pop_grid, pop_display){
    d2 <- dist %>% mutate(tmp = grid.y, grid.y = grid.x, grid.x=tmp) %>% 
        select(-tmp) %>% bind_rows(dist) %>%
        mutate(err=abs(eemsdist-gendist)/(gendist+0.001)) %>%
               group_by(grid.x) %>% summarize(err=median(err)) %>%
               arrange(-err) %>% dplyr::rename(grid=grid.x) %>%
               inner_join(pop_grid) %>%      
               left_join(pop_display) %>%                            
               group_by(grid) %>%                           
               arrange(-n) %>%                              
	       annotate %>%
               summarize(labels=paste(abbrev, collapse="+"),
                         err=mean(err), 
			 color=first_non_grey(color)) %>%
               arrange(-err)
}


plot_error <- function(error, labels='abbrev', nmax=50){
    nmax <- pmin(nmax, nrow(error))
    error <- error[1:nmax,]
    error$order <- as.factor(1:nrow(error))
    error$labels <- error[[labels]]
#    error$labels <- factor(error$labels, 
#			   levels=error$labels[!duplicated(error$labels)])

    if('is_outlier' %in% error){
	cm <- scale_fill_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
	error$color <- error$is_outlier
    } else {
	cm <- scale_fill_identity()
    }

    P <- ggplot(error) + geom_bar(aes(y=err, x=order, 
	      fill=color), stat='identity') + cm +
	      scale_x_discrete(labels=error$labels)
    P <- P + theme_classic(CC$theme_size)
    ymax <- pmin(1, max(error$err))
    P <- P + xlab("") + ylab("Rel. MAD")
#        P <- P + ggtitle("Error by Population")
    P <- P + theme(legend.position=0) 
    P <- P + scale_y_continuous(labels=function(i)sprintf("%.3f", i),
                                limits=c(0,ymax), oob=rescale_none)
#    P <- P + scale_y_continuous(labels=comma_format(digits=2))
#    P <- P + cm +geom_hline(yintercept=0.1, color='lightgray')
#    P <- P + cm +geom_hline(yintercept=0.05, color='lightgray')
    P <- P + theme(axis.text.x = element_text(size=rel(.75), angle=90, vjust=0.5))
}

