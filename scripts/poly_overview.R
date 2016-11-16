suppressPackageStartupMessages({
library(maps)
library(RColorBrewer)
library(dplyr)
})

load_snakemake <- function(){
    poly_files <<- snakemake@input$polys
    pop_geo_files <<- snakemake@input$pop_geo
    pop_display_file <<- snakemake@input$pop_display
    excluded <<- snakemake@input$excluded

    ex <- read.table(excluded)[,1]
    e2 <- data.frame(popId=ex, excluded=T)

    polys <<- lapply(poly_files, read.table)
    disp <<- read.csv(pop_display_file, as.is=T)
    pop_geo <<- lapply(pop_geo_files, read.csv)

    pops <- do.call(rbind, pop_geo)

    pops <- pops %>% left_join(disp)

#    pops <- read.csv("/data/meta/pgs/gvar.pop_geo")
    pops <- pops %>% left_join(e2) 
    pops$excluded[is.na(pops$excluded)] = F       

    pops <- pops %>% arrange(desc(excluded))

    pops <<- unique(cbind(pops$longitude, pops$latitude, pops$excluded, pops$color))
    print(names(pops))
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
    cv <- pops[,4]
    cv[pops[,3] == T] <- 'red'
    points(pops[,1:2], cex=1.5, asp=1, xlab="", ylab="", axes=F, pch=16, col=cv)
    points(pops[,1:2], cex=1.5, pch=1, col='black')
    dev.off()
}

load_snakemake()
plot_polys()
save.image("QQQ.RData")


