suppressPackageStartupMessages({
	library(dplyr)
})

indiv_meta_files <- snakemake@input$indiv_meta

pop_geo <- read.csv(snakemake@input$pop_geo)
pop_display <- read.csv(snakemake@input$pop_display)
outname <- snakemake@output$csv
indiv_meta <- lapply(indiv_meta_files, read.csv)
indiv_all <- do.call(rbind, indiv_meta)
indiv_all %>% left_join(inner_join(pop_geo, pop_display))  %>%
		group_by(popId) %>%
		summarize(name=first(name), 
			  longitude=first(longitude), latitude=first(latitude),
			  sample_size = n_distinct(sampleId),
			  uncertainty = first(accuracy)) %>%
		ungroup() %>% select(-popId) %>%
		write.csv(outname, row.names=F, quote=F)



#load_snakemake()
#plot_polys()
save.image("QQQ.RData")


