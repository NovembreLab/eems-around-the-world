suppressPackageStartupMessages({
	library(ggplot2)
	library(dplyr)
	source("scripts/config.R")
	source("scripts/ggpca2d.R")
})
C <- get_config(snakemake, 'pca2d')
input <- snakemake@input[names(snakemake@input) != '']
output <- snakemake@output[names(snakemake@output) != '']
list2env(input, globalenv())
list2env(output, globalenv())
if(snakemake@params$wdf) C$color <- 'wdf'

data <- load_pca_data(pc, fam, indiv_meta, pop_display)
medians <- read.csv(median)
pg <- read.csv(pop_geo)

data <- left_join(data, pg)
if("order" %in% names(data)) data <- data %>% select(-order) 
data <- data %>% left_join(read.csv(pop_order))



pd <- read.csv(pop_display)
medians <- left_join(medians, pg)
medians <- medians %>% left_join(pd)

if(exists("super_pc")){
#if(C$color == "location_superset"){
    source("scripts/assign_color_by_coord.R")
    super_data <- load_pca_data(super_pc, super_fam, super_indiv_meta, pop_display)
    super_data <- left_join(super_data, pg)

    #distinguish the two sources, used later in map plot
    super_data$shape <- "a"
    data$shape <- "b"

    super_data <- bind_rows(super_data, data)

    super_data$color <- get_cols_wrap(super_data)

    data <- data %>% select(-color) %>% left_join(super_data %>% select(popId, color))
    medians <- medians %>% select(-color) %>% left_join(super_data %>% select(popId, color))
    cv <- as.character(super_data$color)
    names(cv) <- super_data$abbrev
    col <- list(scale_color_manual(values=cv),
		scale_fill_manual(values=cv))
    plot_map(super_data, col, out_map_png, out_map_rds)
    save.image("test")
} else if (C$color == 'wdf'){
    col_list <- data %>% group_by(wasDerivedFrom) %>% 
	summarize(color=first(color), order=mean(order)) %>% 
	arrange(order)
    data$wasDerivedFrom <- factor(data$wasDerivedFrom,
				  levels=col_list$wasDerivedFrom)
    cv <- as.character(col_list$color)
    names(cv) <- col_list$wasDerivedFrom
    col <- list()
} else {
    if(C$color == 'location'){
	source("scripts/assign_color_by_coord.R")
	data$color <- get_cols_wrap(data)
	medians$color <- get_cols_wrap(medians)
    }


    if(C$color == 'exclusion'){
	source("scripts/assign_color_by_coord.R")
	data$color <- get_cols_wrap(data)
	medians$color <- get_cols_wrap(medians)
	excl <- read.csv("subset/excluded.txt") %>% 
	    filter(full==snakemake@wildcards$name)
	data$color[data$popId %in% excl$popId] <- 'red'
	medians$color[medians$popId %in% excl$popId] <- 'red'
    }

    cv <- as.character(medians$color)
    names(cv) <- medians$abbrev
    col <- list(scale_color_manual(values=cv),
		scale_fill_manual(values=cv))

}

if(!exists("super_data")){
    plot_map(medians, col, out_map_png, out_map_rds)
}

#main loop
for(i in seq(1, C$max_n_pc, 2)){
    fig <- make2PC(data, medians, i, i+1, C)
    cur_file <- pc2[[i %/% 2 + 1]]
    cur_rds <- pc2rds[[i %/% 2 + 1]]
    ggsave(cur_file, fig, width=C$width, height=C$height)
    saveRDS(fig, cur_rds)
}
print(warnings())

