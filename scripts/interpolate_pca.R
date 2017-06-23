source("scripts/ggpca.R") 
library(rworldmap)
library(sp)
library(gstat)
suppressPackageStartupMessages({
    library(ggplot2)
    library(magrittr)
})


spatial_subset_plot <- function(subset, PC, ...){
    l <- make_subset_data(subset)
    plot_factor_interpolation(l$data, l$boundary, PC, ...)
}

make_subset_data <- function(subset){
    data <- load.data(sprintf("pca/flash_%s_dim20.pc", subset),
                      sprintf("subset/%s.fam", subset),
                      sprintf("subset/%s.indiv_meta", subset),
                      "/data/meta/pgs/gvar.pop_display")
    loc <- read.csv(sprintf("subset/%s.pop_geo", subset))
    data <- merge(data,loc)
    boundary <- read.table(sprintf("subset/%s.polygon", subset))
    names(boundary) <- c('x', 'y')


    d2 <- aggregate(data, list(data$popId),
                    function(x)ifelse(is.numeric(x), median(x),x[1]) )

    coordinates(d2) <- ~ longitude + latitude
    list(data=d2, boundary=boundary)
}


plot_factor_interpolation <- function(d2, boundary, PC, ...){
    k <- get_interpolation(d2, boundary, PC, ...)

    ggplot() %>%
        add_krig(k, boundary) %>%
        map_plot() 
        #add_original_points(d2, PC)
}

get_fake_colors <- function(data, columns){
    data$max <- apply(data[,columns], 1, which.max)
    print(data$max)
    print("MASDFX")
    n_columns <- length(columns)
    cols <- brewer.pal(n_columns, "Dark2")
    data$col <- NA

    for(i in 1:n_columns){
	COL = names(data[,columns])
	f <- colorRamp(c("white", cols[i]))
	X <- data$max == i
	data$col[X]  <- rgb(f(data[,COL[i]][X])/256)
    }
    data$col
}

plot_factor_interpolation_multi <- function(d2, boundary, ...){
    TESS_COLS <- which(substr(names(d2), 1, 4) == "TESS")
    TESS_COLS_NAMES <- names(d2)[TESS_COLS]
    N_TESS_COLS <- length(TESS_COLS)

    interpolations <- lapply(TESS_COLS_NAMES, function(PC)
			     get_interpolation(data=d2, PC=PC, boundary=boundary) %>%
			     as.data.frame)

    i_red <- interpolations[[1]]
    for(i in interpolations[-1]){
	i_red <- inner_join(i_red, i)
    }

    i_red$col <- get_fake_colors(data=i_red, columns = TESS_COLS_NAMES)
    print("ASDFA")
    
    G <- ggplot() +
	geom_tile(data=i_red, aes(x=x1, y=x2, fill=col), color=NA) + 
	scale_fill_identity() + 
        scale_x_continuous(expand = c(0,0), limits=range(boundary[,1])) +
        scale_y_continuous(expand = c(0,0), limits=range(boundary[,2]))
    G <- map_plot(G) 
    add_original_points_multi(G, d2, TESS_COLS_NAMES)
}


map_plot <- function(G){
    m <- getMap("low")
        G + geom_path(data = m, aes(x=long, y=lat, group = group),            
                             color = 'black') +                 
        coord_map() 
#        geom_polygon(data=boundary, aes(x=x, y=y), fill='red', alpha=.2)
}


add_krig <- function(G, k, boundary, column=3){
    kd <- as.data.frame(k)
    nvar <- as.name(names(kd)[column])
    G + geom_tile(data=kd, aes_(fill=as.name(nvar), x=~x1, y=~x2)) + 
                scale_fill_gradient(low="orange", high="blue") +
        scale_x_continuous(expand = c(0,0), limits=range(boundary[,1])) +
        scale_y_continuous(expand = c(0,0), limits=range(boundary[,2])) 
}

add_original_points_multi <- function(G, data, PC="PC1"){
    data <- as.data.frame(data)
    data$col <- get_fake_colors(data, columns=PC)
    G + geom_point(data=as.data.frame(data),
                   aes(x=longitude, y=latitude, fill=col),
              size=3, shape=21, stroke=1, colour="black") + scale_fill_identity()
}
add_original_points <- function(G, data, PC="PC1"){
    G + geom_point(data=as.data.frame(data),
                   aes_(x=~longitude, y=~latitude, fill=as.name(PC)),
               size=10, shape=21, stroke=2, colour="black") +
                scale_colour_gradient(low="orange", high="blue")
}


get_interpolation <- function(data, boundary, PC="PC1", n=10000, idp=4, maxdist=40,...){
    coordinates(boundary) <- ~ x + y 
    set.seed(1)
    extrapolation_points <- spsample(Polygon(boundary), n=n, 'regular')
#    extrapolation_points <- makegrid(Polygon(boundary), n=n) 

    #lzn.vgm <- variogram(as.name(PC)~1, data=data)                   
    #lzn.fit <- fit.variogram(lzn.vgm, model=vgm(model="Mat"))
    #k <- krige(PC2 ~ 1, data, extrapolation_points, model=lzn.fit) 
    res <-data.frame(coordinates(extrapolation_points),
          sapply(PC, function(P){
            fml <- as.formula(sprintf("%s ~ 1" ,P))
            x <- idw(fml, data, extrapolation_points, idp=idp, maxdist=maxdist,
                     ...) 
            names(x)[names(x) == 'var1.pred'] <- P
            as.data.frame(x)[,P]
           })
          )
    coordinates(res) <- ~ x1 + x2
    res
    }


get_diffs <- function(subset, pc){
    l <- make_subset_data(subset)                     
    pts = get_interpolation(l$data, l$boundary, pc)         


    diffs <- abs(get_diff_grid(pts, pc[1]))
    for(p in pc[-1]){
        diffs <- diffs + abs(get_diff_grid(pts, p))
    }
    l <- list()
    l$pts <- as.data.frame(pts)
    l$diffs <- diffs
    return(l)
}

get_diff_grid <- function(pts, pc){
    print(dim(pts))
    pts <- data.frame(pts, dr=NA, dc=NA)
    row_vals <- unique(pts$x1)
    col_vals <- unique(pts$x2)
    for(r in row_vals){
           is.row <- pts$x1 == r
           row <- pts[is.row,pc]
           pts[is.row,'dr'] <- c(NA, diff(row))
           }
    for(c in col_vals){
           is.col <- pts$x2 == c
           col <- pts[is.col,pc]
           pts[is.col,'dc'] <- c(NA, diff(col))
           }
    return(pts[,c('dr', 'dc')])
}
