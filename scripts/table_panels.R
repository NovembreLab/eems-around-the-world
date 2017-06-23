suppressPackageStartupMessages({
library(maps)
require(dplyr)
library(RColorBrewer)
})

    panels <- snakemake@params$panels
    panel_names <- snakemake@params$names
    panel_abbrev <- snakemake@params$abbrev
    #panels <- grep("[_23]", snakemake@config$subset, invert=T, value=T)
    indiv_meta <- lapply(panels, function(p)
			 read.csv(sprintf("subset/%s.indiv_meta", p)))
    bim <- lapply(panels, function(p)
			 read.table(sprintf("subset/%s.bim", p)))
    n_snps <- sapply(bim, nrow)
    fst <- sapply(panels, function(p)
			 read.table(sprintf("subset/%s.fstall", p))[,1])
    fst <- format(fst, digits=2)
    print("FST:")
    print(fst)
    
    save.image("TEST")

    n_individuals <- sapply(indiv_meta, nrow)

    n_pops <- sapply(indiv_meta, function(i)length(unique(i$popId)))

    #grid <- sapply(panels, function(p) config_eems$eems[[p]]$grid)
    config_eems <-  jsonlite::fromJSON("config/eems.json")   
    grid_res <- sapply(panels, function(i)config_eems$eems[[i]]$grid) * 2
    grid <- sapply(panels, function(p) nrow(read.table(sprintf("eemspilot/0/%s/demes.txt", p))))
#    max_missing <- sapply(panels, function(p) config_subset$subset[[p]]$max_missing)
#    max_missing[sapply(max_missing, is.null)] <- config_subset$subset[['__default__']]$max_missing
    max_missing <- 0

    tbl <- data.frame(panel_names, panel_abbrev,  n_individuals, n_pops, max_missing,n_snps, 
		      grid, grid_res, fst)
    tbl <- tbl %>% arrange(-n_individuals)

    write.csv(tbl, snakemake@output$csv)
    saveRDS(indiv_meta,"paper/panels.rds")
