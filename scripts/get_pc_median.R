suppressPackageStartupMessages({
library(ggplot2)
library(data.table)
library(dplyr)
library(viridis)
source("scripts/load_pop_meta.R")
})
#! called from snakefiles/pca.snake:make_pc_plots

make_pve_plot <- function(pc, output, outrds="pve.rds", nmax=10){
    pve_file <- sprintf("%s.pve", substr(pc, 1, nchar(pc)-3))
    pve <- read.table(pve_file)
    pve <- pve[1:nmax,]
    df <- data.frame(PC=1:length(pve), pve=pve) 
    G <- ggplot(df, aes(y=pve, x=PC)) + geom_bar(stat="identity")  +
	    theme_classic() +
	     theme(axis.text.x = element_text(size=rel(.4), angle = 90, hjust = 1),
              legend.position=0)
    ggsave(output, G, width=7, height=3)
    saveRDS(G, outrds)

}

makePC <- function(data, n, col, field='abbrev'){
    f = sprintf('%s' , field)
    id <- sprintf('PC%d', n)
    G <- ggplot(data, aes_string(f, id, fill=field, color=field))
    G <- G + geom_violin(adjust=.2) + col
    G <- G + theme_classic()
    G <- G + theme(axis.text.x = element_text(size=rel(.4), angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              legend.position=0
              )
    G <- G + theme(legend.position=0)
    G
}

make2PC <- function(data, i, j, col, wdf=F, small=F, for_paper=F, maptoken=NULL,
                    median_only=F){
    size <- ifelse(small, 1, 4)
    id1 <- sprintf('PC%d', i)
    id2 <- sprintf('PC%d', j)
    
    data2 <- data[sample.int(nrow(data), nrow(data)),]

    if(wdf){
        g <- ggplot(data2,aes_string(id1, id2, color='wasDerivedFrom', label='abbrev'))+
            theme_classic() + 
            viridis::scale_color_viridis(discrete=T) + 
            geom_text(size=1.5) + 
            theme(legend.position="bottom",
                  legend.key.size=unit(.5, "cm"),
                  legend.text=element_text(size=5),
                  legend.title=element_blank()) +
            guides(color=guide_legend(nrow=4,byrow=TRUE)) 

    }
    else {
#        g <- ggplot(data2,aes_string(id1, id2,  label='abbrev'), color='lightgray') +
#            geom_text(size=size, color='lightgray') + col
        g <- ggplot(data2,aes_string(id1, id2, colour='abbrev', label='abbrev')) 
        if(!median_only) g <- g + geom_text(size=size, alpha=0.3) 
        g <- g + col + theme_classic()
        g <- g + theme(legend.position='none')
        g <- g + guides(colour=guide_legend(override.aes=list(alpha=1)))
    }


    if(for_paper){
        medians <- data %>% group_by(popId) %>% 
            summarize_at(.cols = vars(starts_with("PC")),
                         .funs=c("M"="median"))
        #medians <- inner_join(medians, other)
        idm1 <- sprintf("%s_M", id1)
        idm2 <- sprintf("%s_M", id2)
        abbrev="abbrev"
        g <- g + geom_point(data=medians, aes_string(x=idm1, y=idm2, 
                                                     col=abbrev), size=10)
        #g <- g+  geom_text(data=medians, aes_string(x=idm1, y=idm2), 
        #              col='white', size=3/4*1.5*1.2*2 ) 


        if(!is.null(maptoken)){
        TOL=3
        require(maps)
        m = map_data("world") %>% filter(region!='Antarctica')
        m$long[m$long< -30] <- m$long[m$long< -30] +360   
        lower_boundary <- m$lat < -38
        m$lat[m$lat< -38] <- -38

        m$lat <- pmin(m$lat, TOL+max(data$latitude))
        m$lat <- pmax(m$lat, -TOL+min(data$latitude))
        m$long <- pmin(m$long, TOL+max(data$longitude))
        m$long <- pmax(m$long, -TOL+min(data$longitude))
        map_bit <- ggplot() +
            geom_path(data=m, aes(x=long, y=lat, group=group), size=0.3 ,color='#222222dd') +
            geom_polygon(data=m, aes(x=long, y=lat, group=group), fill='#eeeeee') +
            geom_point(data=medians, aes(x=longitude, y=latitude,
                                            color=abbrev), size=1) + 
            col +
            coord_fixed() + 
            xlim(range(data$longitude)+ c(-TOL, TOL)) + 
            ylim(range(data$latitude) + c(-TOL, TOL)) +
            scale_x_continuous(expand = c(-.02, -.02))+
            scale_y_continuous(expand = c(-.02, -.02)) +
            theme_classic() +
            theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),legend.position="none",
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank()) +
            theme(legend.position='none')
        ggsave(sprintf('figures/paper/map_%s.png', maptoken), map_bit, width=1.5, height=1.5, dpi=300)
        saveRDS(map_bit, sprintf('figures/paper/map_%s.rds', maptoken))
        }
    }


    g
}


means <- function(data){
    means <- aggregate(data[,-1], list(data$POP), mean)
}

makePlots <- function(data, col, output1, output2, wdf, rdsname="test.rds"){
    token=strsplit(strsplit(output2, "/")[[1]][3], "_")[[1]][2]
    #p_summary <- make2PC(data, 1, 2, col, wdf=wdf, small=T, for_paper=T, maptoken=token)
    #saveRDS(p_summary, rdsname)

    nmax <- sum(substr(names(data),1,2) == 'PC') 
    if(!wdf) p1 <- lapply(1:nmax, function(i) makePC(data, i, col))
    p2 <- lapply(seq(2,nmax, 2), function(i) make2PC(data, i-1, i, col, wdf=wdf))
    #saveRDS(p2, "tmp.debug")
    l = list(PC1=p1, PC2=p2)
    #png(file=output1, width=3200, height=1600)
    #multiplot(plotlist=p1, file=output, cols=4)
    for(i in 1:20){
        if(!wdf)ggsave(output1[i], p1[[i]], width=7, height=3)
    }
    for(i in 1:10){
        ggsave(output2[i], p2[[i]], width=3, height=3)
    }
    p_summary <- list(make2PC(data, 1, 2, col, wdf=wdf, small=T, for_paper=T, maptoken=token),
	make2PC(data, 3, 4, col, wdf=wdf, small=T, for_paper=T, maptoken=token))
    saveRDS(p_summary, rdsname)
}

p1 <- c()
wdf <- F
args <- commandArgs(T)
if(exists('snakemake')){
    pc <- snakemake@input[['pc']]
    fam <- snakemake@input[['fam']]
    indiv_meta <- snakemake@input[['indiv_meta']]
    pop_display <- snakemake@input[['pop_display']]
    output <- snakemake@output[['pc']]
    data <- load_pca_data(pc, fam, indiv_meta, pop_display)

    medians <- data %>% group_by(popId) %>% 
        summarize_at(.cols = vars(starts_with("PC")),
                     .funs=c("M"="median"))
#    other <- data %>% group_by(popId) %>%
#        summarize(abbrev=first(abbrev),
#                  latitude=median(latitude), longitude=median(longitude))
#    medians <- inner_join(medians, other)
    write.csv(medians, output, row.names=F)


}


warnings()





