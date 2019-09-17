library(yaml)
library(dplyr)
library(magrittr)
source("scripts/load_pop_meta.R") #load raw




ggadd_annotation <- function(map, label="hg"){
    # LOAD STUFF
    C <- yaml.load_file("config/plots.yaml")$plot$annotations

    popgeo <- "pgs/gvar3.pop_geo"
    popdisplay <- "pgs/gvar3.pop_display"
    y <- yaml.load_file("config/data.yaml")
    filters <- y$filter
    anno <- y$anno
    pm <- load_pop_meta(popgeo, popdisplay)
    a <- anno[[label]]


    # FILTER STUFF
    ff0 <- a$from_filter
    ff <- if(is.null(ff0)) NULL else filters[[ff0]]
    filter_ids <- pm %>% filter(popId %in% ff)

    direct_ids <- pm %>% filter(popId %in% a$id)
    abbrevs <- pm %>% filter(abbrev %in% a$abbrev)

    to_annotate <- bind_rows(filter_ids, direct_ids, abbrevs)


    #now the plotting bit
    if(label %in% names(C)){
	C <- C[[label]]
    } else {
	C <- C[["__default__"]]
    }
    C$pm <- to_annotate
    C$map <- map
    do.call(.gg_add_samples_true, C)
}


