suppressPackageStartupMessages({
	library(dplyr)
})


indiv_meta_files <- snakemake@input$indiv_meta
outname <- snakemake@output$csv
indiv_meta <- lapply(indiv_meta_files, read.csv)
indiv_all <- do.call(rbind, indiv_meta)

pub <- read.csv(snakemake@input$pub_label)
pub$pub <- as.character(pub$pub)

indiv_all %>% filter(!duplicated(indiv_all$sampleId)) %>%
        left_join(pub) %>%
	group_by(pub) %>%
	summarize(n_samp=length(sampleId), n_pops=n_distinct(popId)) %>%
    arrange(pub) %>%
	write.csv(outname, row.names=F, quote=F)



#load_snakemake()
#plot_polys()
save.image("QQQ.RData")


