suppressPackageStartupMessages({
library(scales)
library(RColorBrewer)
library(dplyr)
})
get_cols <- function(axis1, axis2, cmat=color_mat3()){
    x = floor(100 * (axis1 - min(axis1))/diff(range(axis1)) )+1
    y = floor(100 * (axis2 - min(axis2))/diff(range(axis2)) )+1
    cv <- cmat[cbind(x, y)]
}

color_mat <- function(col_top=c("black", "darkblue", "#008080", "darkgreen",
                                "#6dab26"),
                      col_bot=c("gray60", "purple", '#802040','#800000','#aa4000',
                                'darkorange')          
                      ){
    grad_top <- rev(gradient_n_pal(col_top)(0:100/100))
    grad_bot <- rev(gradient_n_pal(col_bot)(0:100/100))
    cols <- t(mapply(function(x,y)gradient_n_pal(c(x,y))(0:100/100),
                    grad_bot, grad_top))
}

color_mat3 <- function(col_top=c("darkgreen", "#008080",  "darkblue"),
                           col_mid=c("#dddd00", "grey40", "#8d0fd2"),
                           col_bot=c('darkorange',  "#800000","black" )){
    grad_top <- rev(gradient_n_pal(col_top)(0:100/100))
    grad_bot <- rev(gradient_n_pal(col_bot)(0:100/100))
    grad_mid <- rev(gradient_n_pal(col_mid)(0:100/100))
    cols <- t(mapply(function(x,y)gradient_n_pal(c(x,y))(0:50/50),
                    grad_mid, grad_top))
    cols2 <- t(mapply(function(x,y)gradient_n_pal(c(x,y))(0:50/50),
                    grad_bot, grad_mid))
    cols <- cbind(cols2,cols)
    t(cols)
}
get_cols_wrap <-function(pop_geo, cmat=color_mat3()){
    to_wrap <- pop_geo$longitude < -34
    to_wrap[is.na(to_wrap)] <- F
    saveRDS(pop_geo, "DEBUG.rds")
    pop_geo$longitude[to_wrap] <-pop_geo$longitude[to_wrap]  + 360
    cols <- get_cols(pop_geo$longitude, pop_geo$latitude)
    cols <- get_cols(pmin(pop_geo$longitude, 180), pop_geo$latitude)
    to_wrap <- pop_geo$longitude > 180
    to_wrap[is.na(to_wrap)] <- F
    pop_geo$longitude[to_wrap] <-pop_geo$longitude[to_wrap]  - 360
    print(length(cols))
    cols
}



