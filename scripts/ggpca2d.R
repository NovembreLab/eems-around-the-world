suppressPackageStartupMessages({
library(ggplot2)
library(data.table)
library(dplyr)
library(viridis)
library(ggrepel)
source("scripts/load_pop_meta.R")
source("scripts/themes.R")
})
#! called from snakefiles/pca.snake:make_pc_plots



make2PC <- function(data, medians=NULL, i=1, j=2, C=list()){
    id1 <- sprintf('PC%d', i)
    id2 <- sprintf('PC%d', j)
    
    data <- data[sample.int(nrow(data), nrow(data)),]

    if(C$color == 'wdf'){
        g <- ggplot(data2,aes_string(id1, id2, color='wasDerivedFrom', label='abbrev'))+
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
        g <- ggplot(data, aes_string(id1, id2, color='color', 
                                         label=C$text_field),
                    alpha=C$point_alpha)


        #second: points or text?
        if(C$point_type== 'text'){

            g <- g + geom_text(size=C$text_size)

        } else if(C$point_type== 'points'){
            g <- g + geom_point(size=C$text_size)
        }

        idm1 <- sprintf("%s_M", id1)
        idm2 <- sprintf("%s_M", id2)
	if(F){
        if(C$median_label){
            if(C$median_label_grey) medians$color <- '#404040'
            g <- g+  geom_text(data=medians, aes_string(x=idm1, y=idm2, 
                          color='color'),
                          size=C$median_size, alpha=C$median_alpha ) 
    }}
    if(C$median){
        g <- g + geom_point(data=medians, 
                            aes_string(x=idm1, y=idm2,color='color'), 
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
		     label.size= unit(0, "lines"),
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
		     label.size= unit(0, "lines"),
			  
		        point.padding = unit(0.001, "lines")
		          )	
	} 

        g <- g + pca_2d_theme(base_size=C$theme_size)

    }
}

plot_map <- function(medians, col, outpng, outrds){
    TOL=2
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
        map_inlet_theme(base_size=C$theme_size)
    ggsave(outpng, map_bit, width=1.5, height=1.5, dpi=300)
    saveRDS(map_bit, outrds)
}




