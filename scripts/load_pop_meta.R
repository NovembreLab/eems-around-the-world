require(dplyr)
require(data.table)

load_pop_meta <- function(pop_geo_file, pop_display_file){
	pg <- read.csv(pop_geo_file)
	pd <- read.csv(pop_display_file)
	pg %>% inner_join(pd)
}

load_pca_data <- function(pc, fam,
                      indiv_meta, pop_display){
    indiv_meta <- read.csv(indiv_meta)
    pop_display <- read.csv(pop_display)
    indiv <- merge(indiv_meta, pop_display, all.x=T)

    fam <- read.table(fam)[,1]

    data <- data.frame(fread(pc))
    names(data) <- paste0("PC", 1:ncol(data))

    data <- cbind(fam, data, n=1:length(fam)) 
    names(data)[1] <- 'sampleId'
    m <- merge(indiv, data, all.y=T)
    m <- m[order(m$n),]

    return(m)
}
