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

    ex <- read.csv(excluded)[,1]
    e2 <- data.frame(popId=ex, excluded=T)

    polys <<- lapply(poly_files, read.table)
    disp <<- read.csv(pop_display_file, as.is=T)
    pop_geo <<- lapply(pop_geo_files, read.csv)

    pops <- do.call(rbind, pop_geo)

    pops$popId <- as.character(pops$popId)
    disp$popId <- as.character(disp$popId)
    e2$popId <- as.character(e2$popId)
    pops <- pops %>% left_join(disp)

#    pops <- read.csv("/data/meta/pgs/gvar.pop_geo")
    print(names(e2))
    pops <- pops %>% left_join(e2) 
    print(sum(pops$excluded, na.rm=T))
    pops$excluded[is.na(pops$excluded)] = F       

    pops <- pops %>% arrange(desc(excluded))

    pops <<- unique(cbind(pops$longitude, pops$latitude, pops$excluded, pops$color))
    out_png <<- snakemake@output$png
    return(pops)
}


plot_polys <- function(pops){
	COORDS=c("longitude", "latitude")
	print(dim(pops))
    pdf(file=out_png, width=16, height=7)
    palette(brewer.pal(12,"Set3"))
    par(mar=c(0,0,0,0))
    print(range(pops[,1]))
    plot(pops[,COORDS], asp=1, xlab="", ylab="", axes=F, pch=16, col=NULL)

    n <- length(polys)
    for(i in 1:n){
	    polygon(polys[[i]], border=i,
		    col=rgb(t(col2rgb(i))/256, alpha=.5), 
		    lwd=2, lty=2)
    }

    m <- map(add=T, col='black')
    m$x <- m$x+360
    lines(m, col='black')
    cv <- pops$color
    cv[pops[,"excluded"] == T] <- 'red'
    cv <- adjustcolor(cv, alpha.f=0.7)
    pch <-  rep(16,nrow(pops))
    pch[pops[,"excluded"] == T] <- 15
    print(table(pch))
    pch_around <-  rep(1,nrow(pops))
    pch_around[pops$excluded == T] <- 22
    points(pops[,COORDS], cex=1.5, asp=1, xlab="", ylab="", axes=F, pch=pch, col=cv)
    points(pops[,COORDS], cex=1.5, pch=pch_around, col='black')
    #points(pops[,COORDS], col="red")
    dev.off()
}

pops <- load_snakemake()
plot_polys(pops)
save.image("QQQ.RData")


