library(gridExtra)

source("scripts/tessplot.r")
source("scripts/interpolate_pca.R")

load.data <- function(pc, fam,                           
                      indiv_meta, pop_display){          
    indiv_meta <- read.csv(indiv_meta)                   
    pop_display <- read.csv(pop_display)                 
    indiv <- merge(indiv_meta, pop_display, all.x=T)     
                                                         
    fam <- read.table(fam)[,1]                           
                                                         
    data <- data.frame(fread(pc))                        
    names(data) <- paste0("TESSPOP", 1:ncol(data))            
                                                         
    data <- cbind(fam, data, n=1:length(fam))            
    names(data)[1] <- 'sampleId'                         
    m <- merge(indiv, data, all.y=T)                     
    m <- m[order(m$n),]                                  
                                                         
    return(m)                                            
}                                                        


make_subset_data <- function(subset, n=3,
	pop_display="/data/popres_data/popres.pop_display",
	run=0){
    data <- load.data(sprintf("tess/%s.%s_run%s.Q", subset, n, run),
                      sprintf("%s.fam", subset),
                      sprintf("%s.indiv_meta", subset),
                      pop_display)
    loc <- read.csv(sprintf("%s.pop_geo", subset))
    data <- merge(data,loc)
    boundary <- read.table(sprintf("%s.polygon", subset))
    names(boundary) <- c('x', 'y')


    d2 <- aggregate(data, list(data$popId),
                    function(x)ifelse(is.numeric(x), median(x),x[1]) )

    coordinates(d2) <- ~ longitude + latitude
    list(data=d2, boundary=boundary)
}

plot_tess <- function(subset, n, pop_display, ...){ 
    n <- as.numeric(n)
    d <- make_subset_data(subset, n, pop_display)
    l <- lapply(1:n, function(i){
	col <- sprintf('TESSPOP%s', i) 
        plot_factor_interpolation(d$data, d$boundary, col)
	})

    do.call(arrangeGrob,l)
    
}

args <- commandArgs(T)
print(args)
if(length(args)>3){
    n = as.numeric(args[3])
    p <- plot_tess(subset=args[2], n=n,
        pop_display=args[1], run=args[4])
    nrow = ceiling(n/2)
    ggsave(args[5], p, width=8, height = 4 * nrow)
}
