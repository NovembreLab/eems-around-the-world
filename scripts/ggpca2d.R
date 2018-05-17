suppressPackageStartupMessages({
library(ggplot2)
library(data.table)
library(dplyr)
library(viridis)
library(ggrepel)
source("scripts/load_pop_meta.R")
source("scripts/themes.R")
})
#!  called from snakefiles/pca.snake:make_pc_plots




make2PC <- function(data, medians=NULL, i=1, j=2, C=list(), pve_data=NULL){
    id1 <- sprintf('PC%d', i)
    id2 <- sprintf('PC%d', j)
    idm1 <- sprintf("%s_M", id1)
    idm2 <- sprintf("%s_M", id2)

    if (i %in% C$flip){data[,id1] <- -data[,id1]}
    if (j %in% C$flip){data[,id2] <- -data[,id2]}
    if (i %in% C$flip){medians[,idm1] <- -medians[,idm1]}
    if (j %in% C$flip){medians[,idm2] <- -medians[,idm2]}
    if(C$rotate){tmp <- id1; id1 <- id2; id2 <- tmp}
    if(C$rotate){tmp <- idm1; idm1 <- idm2; idm2 <- tmp}
    if(C$rotate){tmp <- i; i <- j; j <- tmp}
    
    data <- data[sample.int(nrow(data), nrow(data)),]



    if(!"shape" %in% names(data)) data$shape <- "a"
    if(!"shape" %in% names(medians)) medians$shape <- "a"

    if(C$color == 'wdf'){
        g <- ggplot(data2,aes_string(id1, id2, color='source', label='abbrev'))+
            theme_classic() + 
            viridis::scale_color_viridis(discrete=T) + 
            theme(legend.position="bottom",
                  legend.key.size=unit(.5, "cm"),
                  legend.text=element_text(size=5),
                  legend.title=element_blank()) +
            guides(color=guide_legend(nrow=4,byrow=TRUE)) 

    }
    else {
        #first step: is color grey or data-derived?
        if(C$point_grey) data$color <- "#bbbbbb"
        g <- ggplot(data, aes_string(id1, id2, color='color', shape="shape",
                                         label=C$text_field),
                    alpha=C$point_alpha)


        #second: points or text?
        if(C$point_type== 'text'){

            g <- g + geom_text(size=C$text_size)

        } else if(C$point_type== 'points'){
            g <- g + geom_point(size=C$text_size)
        }

	if(F){
        if(C$median_label){
            if(C$median_label_grey) medians$color <- '#404040'
            g <- g+  geom_text(data=medians, aes_string(x=idm1, y=idm2, 
                          color='color'),
                          size=C$median_size, alpha=C$median_alpha ) 
    }}
    if(C$median){
        g <- g + geom_point(data=medians, 
                            aes_string(x=idm1, y=idm2,color='color', shape="shape"), 
                            size=C$median_size, alpha=C$median_alpha)
    }

	if(C$median_label && C$median_repel){
        print("median label")

           g <- g + geom_label_repel(data=medians,
                                aes_string(x=idm1, y=idm2
					    ), 
                            size=C$median_label_size, alpha=C$median_alpha,
		     fill=NA,#"#dddddd50",
		     label.padding = unit(C$median_label_size/20, "lines"),
		     box.padding = unit(0.001, "lines"),
		     label.r = unit(0.001, "lines"),
		     label.size= unit(0.00001, "lines"),
		     segment.size = 0.1,
		     segment.type = 2,
			  
		        point.padding = unit(0.001, "lines")
		          )	
	} 
	if(C$median_label && !C$median_repel){
        print("median label no repel")

           g <- g + geom_label(data=medians,
                                aes_string(x=idm1, y=idm2
					    ), 
                            size=C$median_label_size, alpha=C$median_alpha,
		     fill=NA,#"#dddddd50",
		     label.padding = unit(C$median_label_size/20, "lines"),
		     box.padding = unit(0.001, "lines"),
		     label.r = unit(0.001, "lines"),
		     label.size= unit(0.00001, "lines"),
			  
		        point.padding = unit(0.001, "lines")
		          )	
	} 

        #rename ids s.t. percent variance explained is plotted is added
        #probably could use a rename all treatment
        if(!is.null(pve_data)){
            id1_label <- sprintf("%s (%2.2f%%)", id1, pve_data[i]*100)
            id2_label <- sprintf("%s (%2.2f%%)", id2, pve_data[j]*100)
            g <- g + xlab(id1_label) + ylab(id2_label)
        }

        g <- g + pca_2d_theme(base_size=C$theme_size)

    }
}

plot_map <- function(medians, col, outpng, outrds){
    TOL=2
    print("_-------------")
    print(names(medians))
    if(!"shape" %in% names(medians)) medians$shape <- "a"
    require(maps)
    m = map_data("world") %>% filter(region!='Antarctica')
    m$long[m$long< -30] <- m$long[m$long< -30] +360   
    lower_boundary <- m$lat < -38
    m$lat[m$lat< -38] <- -38

    m$lat <- pmin(m$lat, TOL+max(medians$latitude))
    m$lat <- pmax(m$lat, -TOL+min(medians$latitude))
    m$long <- pmin(m$long, TOL+max(medians$longitude))
    m$long <- pmax(m$long, -TOL+min(medians$longitude))
    map_bit <- ggplot() +
	geom_path(data=m, aes(x=long, y=lat, group=group), size=0.3 ,color='#222222dd') +
	geom_polygon(data=m, aes(x=long, y=lat, group=group), fill='#eeeeee') +
	geom_point(data=medians, aes(x=longitude, y=latitude, shape=shape,
					color=abbrev), size=2) + 
	col +
	coord_fixed() + 
	xlim(range(data$longitude)+ c(-TOL, TOL)) + 
	ylim(range(data$latitude) + c(-TOL, TOL)) +
	scale_x_continuous(expand = c(-.02, -.02))+
	scale_y_continuous(expand = c(-.02, -.02)) +
        map_inlet_theme(base_size=C$theme_size)
    ggsave(outpng, map_bit, width=1.5, height=1.5, dpi=300)
    saveRDS(map_bit, outrds)
}
