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


make_subset_data <- function(subset){
    data <- load.data(sprintf("tess/subset/%s.3_run0.Q", subset),
                      sprintf("subset/%s.fam", subset),
                      sprintf("subset/%s.indiv_meta", subset),
                      "/data/popres_data/popres.pop_display")
    loc <- read.csv(sprintf("subset/%s.pop_geo", subset))
    data <- merge(data,loc)
    boundary <- read.table(sprintf("subset/%s.polygon", subset))
    names(boundary) <- c('x', 'y')


    d2 <- aggregate(data, list(data$popId),
                    function(x)ifelse(is.numeric(x), median(x),x[1]) )

    coordinates(d2) <- ~ longitude + latitude
    list(data=d2, boundary=boundary)
}
