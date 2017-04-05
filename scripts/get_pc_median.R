suppressPackageStartupMessages({
library(dplyr)
source("scripts/load_pop_meta.R")
})
#! called from snakefiles/pca.snake:make_pc_plots

pc <- snakemake@input[['pc']]
fam <- snakemake@input[['fam']]
indiv_meta <- snakemake@input[['indiv_meta']]
pop_display <- snakemake@input[['pop_display']]
output <- snakemake@output[['pc']]
data <- load_pca_data(pc, fam, indiv_meta, pop_display)

medians <- data %>% group_by(popId) %>% 
    summarize_at(.cols = vars(starts_with("PC")),
		 .funs=c("M"="median"))
write.csv(medians, output, row.names=F)
