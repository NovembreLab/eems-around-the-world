library(dplyr)
#library(readr)


if(exists("snakemake")){
	pc_name <- snakemake@input$pc
	ind_name <- snakemake@input$inds
	pop_name <- snakemake@input$pop
	out_name <- snakemake@output$pop_order

}


pcs <- read.table(pc_name)
names(pcs) <- sprintf("PC%s", 1:ncol(pcs))

inds <- data.frame(sampleId=read.table(ind_name, as.is=T)[,1])
pop <- read.csv(pop_name)

pop_tbl <- inds %>% bind_cols(pcs) %>% left_join(pop) %>%
	group_by(popId) %>%
	summarize(order=median(PC1)) %>% arrange(order) %>% 
	write.csv(out_name, row.names=F, quote=F)


