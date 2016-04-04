library(scales)
library(shiny)
library(RColorBrewer)

pop_geo <- read.csv("pgs/gvar.pop_geo")
pop_display <- read.csv("pgs/gvar.pop_display")

color_mat <- function(col_top=c("gray80", "purple", "darkblue", "darkgreen", 'yellow'),
                      col_bot=c("black", "pink", 'red', 'orange')
                      ){
    grad_top <- rev(gradient_n_pal(col_top)(0:100/100))
    grad_bot <- rev(gradient_n_pal(col_bot)(0:100/100))
    cols <- t(mapply(function(x,y)gradient_n_pal(c(x,y))(0:100/100),
                    grad_bot, grad_top))
}

#! get colors from a 2d color grid
get_cols <- function(axis1, axis2, cmat=color_mat()){
    x = floor(100 * (axis1 - min(axis1))/diff(range(axis1)) )+1
    y = floor(100 * (axis2 - min(axis2))/diff(range(axis2)) )+1
    cv <- cmat[cbind(x, y)]
}

get_cols_wrap <-function(pop_geo, cmat=color_mat()){
    to_wrap <- pop_geo$longitude < -34
    pop_geo$longitude[to_wrap] <-pop_geo$longitude[to_wrap]  + 360
    cols <- get_cols(pop_geo$longitude, pop_geo$latitude)
    to_wrap <- pop_geo$longitude > 180
    pop_geo$longitude[to_wrap] <-pop_geo$longitude[to_wrap]  - 360
    print(length(cols))
    cols
}


get_cols_source <- function(data){
    a <- read.csv("pgs/gvar.indiv_prov")
    l <- levels(a$wasDerivedFrom)
    cmap = brewer.pal(8, 'Set1')
    
    f <- factor(data$wasDerivedFrom, levels=l)
    cmap[f]
}
                          

render_leaflet_map <- function( ...) renderLeaflet({
    m = getMap('low')
    map_leaflet = leaflet(m) %>% addTiles() %>%
    addCircleMarkers(lat=x$latitude, lng=x$longitude,
            color=x$color, radius=(x$accuracy/20+8),
            )                                                              
}, ...)

render_leaflet_pc2d <- function(x, input,...)renderLeaflet({
    PC1 = paste0('PC', as.integer(1))
    PC2 = paste0('PC', as.integer(2))
    if(input$coloring == 'Source'){
	pc2d_leaflet =leaflet(x) %>%
	addCircleMarkers(lng = x[,PC1], lat = x[,PC2],
			 label = ~wasDerivedFrom, color= ~color)
    } else {
	pc2d_leaflet =leaflet(x) %>%
	addCircleMarkers(lng = x[,PC1], lat = x[,PC2],
			 label = ~name, color= ~color)
	}
}, ...)
