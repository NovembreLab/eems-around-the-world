suppressPackageStartupMessages({
library(maps)
library(RColorBrewer)
library(dplyr)
})

load_snakemake <- function(){
    poly_files <<- snakemake@input$polys
    pop_geo_files <<- snakemake@input$pop_geo
    excluded <<- snakemake@input$excluded

    ex <- read.table(excluded)[,1]
    e2 <- data.frame(popId=ex, excluded=T)

    polys <<- lapply(poly_files, read.table)
    pop_geo <<- lapply(pop_geo_files, read.csv)

    pops <- do.call(rbind, pop_geo)
    pops <- pops %>% left_join(e2)             
    pops$excluded[is.na(pops$excluded)] = F       

    pops <<- unique(cbind(pops$longitude, pops$latitude, pops$excluded))
    out_png <<- snakemake@output$png
}


plot_polys <- function(){
    pdf(file=out_png, width=16, height=7)
    palette(brewer.pal(12,"Set3"))
    par(mar=c(0,0,0,0))
    plot(pops[,1:2], asp=1, xlab="", ylab="", axes=F, pch=16, col=NULL)

    n <- length(polys)
    for(i in 1:n){
	    polygon(polys[[i]], border=i,
		    col=rgb(t(col2rgb(i))/256, alpha=.5), 
		    lwd=2, lty=2)
    }

    m <- map(add=T, col='black')
    m$x <- m$x+360
    lines(m, col='black')
    cv <- c('black', 'red')[pops[,3]+1]
    points(pops[,1:2], asp=1, xlab="", ylab="", axes=F, pch=16, col=cv)
    dev.off()
}

load_snakemake()
plot_polys()
save.image("QQQ.RData")


