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
data <- data %>% select(-order) %>% left_join(read.csv(pop_order))
medians <- left_join(medians, pg) %>% left_join(read.csv(pop_display))

if (C$color == 'wdf'){
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

    cv <- as.character(medians$color)
    names(cv) <- medians$abbrev
    col <- list(scale_color_manual(values=cv),
		scale_fill_manual(values=cv))

}
plot_map(medians, out_map_png, out_map_rds)

#main loop
for(i in seq(1, C$max_n_pc, 2)){
    fig <- make2PC(data, medians, i, i+1, C)
    cur_file <- pc2[[i %/% 2 + 1]]
    cur_rds <- pc2rds[[i %/% 2 + 1]]
    ggsave(cur_file, fig, width=C$width, height=C$width)
    saveRDS(fig, cur_rds)
}

