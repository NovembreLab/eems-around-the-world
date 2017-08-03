suppressPackageStartupMessages({
	library(dplyr)
	library(yaml)
})
filter <- yaml::yaml.load_file("config/data.yaml")$filter
print(snakemake@input$indiv_meta0)
indiv_meta0 <- read.csv(snakemake@input$indiv_meta0)

hg <- indiv_meta0 %>% filter(popId %in% filter$hg)
hg$panel <- "HG"

mig <- indiv_meta0 %>% filter(popId %in% filter$recent_migrant)
mig$panel <- "ADMIX"

pub <- read.csv(snakemake@input$pub_label)


#indiv_meta_files <- snakemake@input$indiv_meta
indiv_meta_files <- sprintf("subset/%s.indiv_meta", snakemake@params$panels)

pop_geo <- read.csv(snakemake@input$pop_geo)
pop_display <- read.csv(snakemake@input$pop_display)
outname <- snakemake@output$csv
indiv_meta <- lapply(indiv_meta_files, read.csv, strings=F)

indiv_all <-bind_rows(indiv_meta, .id="panel")
indiv_all$panel <- snakemake@params$abbrev[as.numeric(indiv_all$panel)]

indiv_all <- bind_rows(list(indiv_all, hg, mig))


indiv_all %>% left_join(pub) %>% 
        left_join(inner_join(pop_geo, pop_display))  %>%
		group_by(popId) %>%
		summarize(abbrev=as.character(first(abbrev)), name=first(name), 
              src=paste(sort(unique(pub)), collapse="|"),
			  longitude=first(longitude), latitude=first(latitude),
			  sample_size = n_distinct(sampleId),
			  panel=paste(unique(panel), collapse="|")
			  ) %>% 
		ungroup() %>% select(-popId) %>%
		arrange(abbrev) -> x
		write.csv(x, outname, row.names=F, quote=T)



#load_snakemake()
#plot_polys()
save.image("test")


