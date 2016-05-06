require(rworldmap)
require(methods)

WRAP_PT = -31

plot_worldmap <- function(...){
    m <- getMap('low')
    for(i in 1:244){
        x <- m@polygons[[i]]@Polygons
        for(j in 1:length(x)){
            k <- x[[j]]@coords 
            r <- range(k[,1])
            if(r[[1]] < WRAP_PT && r[[2]] > WRAP_PT){
                k[,1] <- pmin(k[,1]+360, -WRAP_PT+360)
                print(c("gaga", i, j))
            }
            if(r[[2]] < WRAP_PT){
                k[,1] <- k[,1] + 360
            }
            m@polygons[[i]]@Polygons[[j]]@coords  <- k
        }
    }

    #plot(NA, xlim=c(-31, -31+360), ylim=c(-50,75), xaxs='i', 
    #     xlab='', ylab='', xaxt='n', yaxt='n', ...) 
    plot(m, add=T, ...)                                           
}

