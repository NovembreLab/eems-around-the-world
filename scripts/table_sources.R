suppressPackageStartupMessages({
	library(dplyr)
})


indiv_meta_files <- snakemake@input$indiv_meta
outname <- snakemake@output$csv
indiv_meta <- lapply(indiv_meta_files, read.csv)
indiv_all <- do.call(rbind, indiv_meta)

indiv_all %>% filter(!duplicated(indiv_all$sampleId)) %>%
	group_by(wasDerivedFrom) %>%
	summarize(n_samp=length(sampleId), n_pops=n_distinct(popId)) %>%
    arrange(-n_samp) %>%
	write.csv(outname, row.names=F, quote=F)



#load_snakemake()
#plot_polys()
save.image("QQQ.RData")


