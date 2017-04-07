suppressPackageStartupMessages({
library(dplyr)
library(reshape2)
require(fields)
source("scripts/load_pop_meta.R")
})

f <- function(x, n){
    median(rep(x, n))
}

pcs_file <- snakemake@input$pcs
popgrid_file <- snakemake@input$popgrid
outname <- snakemake@output[[1]]
npc <- snakemake@wildcards$npc

pc_name <- sprintf('pcDist%s', npc) 

n_pcs <- snakemake@wildcards$npc

pc_medians <- read.csv(pcs_file)
popgrid <- read.csv(popgrid_file)
#pc_medians %>% left_join(popgrid) %>% group_by(grid) %>% saveRDS("test.rds")
pc_medians %>% left_join(popgrid) %>% group_by(grid) %>%
    filter(!is.na(grid)) %>%
    summarize_at(.cols=vars(starts_with("PC")), .funs=
		 funs(f(., n=n))
		 ) %>% ungroup  -> pc_medians
#rownames(pc_medians) <- pc_medians$grid
pc_medians %>% arrange(grid) %>% 
    select(-grid) -> pc_medians

dmat <- as.matrix(dist(pc_medians[,1:n_pcs]))
v <- melt(dmat)
names(v) <- c('grid.x', 'grid.y', pc_name)
v <- v %>% filter(grid.x < grid.y)
write.csv(v, outname, row.names=F)

