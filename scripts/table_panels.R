suppressPackageStartupMessages({
library(maps)
library(RColorBrewer)
})

load_snakemake <- function(){
    panels <- snakemake@params$panels
    print(panels)
    #panels <- grep("[_23]", snakemake@config$subset, invert=T, value=T)
    indiv_meta <- lapply(panels, function(p)
			 read.csv(sprintf("subset/%s.indiv_meta", p)))
    bim <- lapply(panels, function(p)
			 read.table(sprintf("subset/%s.bim", p)))
    n_snps <- sapply(bim, nrow)
    n_individuals <- sapply(indiv_meta, nrow)
    n_pops <- sapply(indiv_meta, function(i)length(unique(i$popId)))
    config_eems <-  jsonlite::fromJSON("config/eems.json")   
    config_subset <-  jsonlite::fromJSON("config/subset.json")   
    #grid <- sapply(panels, function(p) config_eems$eems[[p]]$grid)
    grid <- sapply(panels, function(p) nrow(read.table(sprintf("eemsout/0/%s/demes.txt", p))))
#    max_missing <- sapply(panels, function(p) config_subset$subset[[p]]$max_missing)
#    max_missing[sapply(max_missing, is.null)] <- config_subset$subset[['__default__']]$max_missing
    max_missing <- 0

    tbl <- cbind(panels,  n_individuals, n_pops, max_missing,n_snps, grid)

    write.csv(tbl, snakemake@output$csv)
}
load_snakemake()



#load_snakemake()
#plot_polys()
save.image("QQQ.RData")


