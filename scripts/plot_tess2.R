library(gridExtra)
library(dplyr)
library(RColorBrewer)

source("scripts/tessplot.r")
source("scripts/interpolate_pca.R")

load.data <- function(pc, fam,                           
                      indiv_meta, pop_display){          
    indiv_meta <- read.csv(indiv_meta)                   
    pop_display <- read.csv(pop_display)                 
    indiv <- merge(indiv_meta, pop_display, all.x=T)     
                                                         
    fam <- read.table(fam)[,1]                           
    print(length(fam))
                                                         
    data <- data.frame(fread(pc))                        
    data <- data[1:length(fam),]
    names(data) <- paste0("TESSPOP", 1:ncol(data))            
                                                         
    data <- cbind(fam, data, n=1:length(fam))            
    names(data)[1] <- 'sampleId'                         
    m <- merge(indiv, data, all.y=T)                     
    m <- m[order(m$n),]                                  
                                                         
    return(m)                                            
}                                                        


make_subset_data <- function(snakemake, pop_display){
    data <- load.data(snakemake@input$tess,
                      snakemake@input$fam,
                      snakemake@input$indiv_meta,
                      pop_display)
    loc <- read.csv(snakemake@input$pop_geo)
    data <- merge(data,loc)
    boundary <- read.table(snakemake@input$boundary)
    names(boundary) <- c('x', 'y')


    d2 <- aggregate(data, list(data$popId),
                    function(x)ifelse(is.numeric(x), median(x),x[1]) )

    coordinates(d2) <- ~ longitude + latitude
    list(data=d2, boundary=boundary)
}

plot_tess <- function(snakemake, pop_display, ...){ 
    n <- as.numeric(n)
    d <- make_subset_data(snakemake, pop_display)
    l <- lapply(1:n, function(i){
        col <- sprintf('TESSPOP%s', i) 
        plot_factor_interpolation(d$data, d$boundary, col)
	})

    do.call(arrangeGrob,l)
}

plot_tess_multi <- function(snakemake, pop_display, ...){ 
    n <- as.numeric(n)
    d <- make_subset_data(snakemake, pop_display)
    plot_factor_interpolation_multi(d$data, d$boundary)

}

n <- as.numeric(snakemake@wildcards$n)
subset <- snakemake@input$tess
pop_display <- snakemake@input$pop_display
run <- as.numeric(snakemake@wildcards$run)
outfile <- snakemake@output$plot
nrow = 1

p <- plot_tess_multi(snakemake,
    pop_display=pop_display)
ggsave(outfile, p, width=8, height = 4 * nrow)
