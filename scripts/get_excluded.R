library(dplyr)

old <- function(){
C <- snakemake@config$paper
panels <- names(C)

outfile <- snakemake@output$excluded
print(outfile)

excluded_table <- data.frame(panel=c(),
			     popId=c(),
			     abbrev=c(),
			     N=c())

pd <- read.csv(snakemake@input$pop_display)

for(panel in panels){
    print(panel)
#panel <- 'Southern Africa'
    main = C[[panel]][['main']]
    full = C[[panel]][['full']]
    if(is.null(full)){print("X!"); next};
    if(full == F){print("X2"); next};

    pg_m <- read.csv(sprintf("subset/%s.pop_geo", main))
    pg_f <- read.csv(sprintf("subset/%s.pop_geo", full))
    im_f <- read.csv(sprintf("subset/%s.indiv_meta", full))
    excluded_ids <- setdiff(pg_f %>% select(popId), pg_m %>% select(popId))

    n_excluded <-  excluded_ids %>% left_join(im_f) %>% 
	    group_by(popId) %>% summarize(N=n())
    tbl <- n_excluded %>% left_join(select(pd, popId, abbrev))
    tbl$panel <- panel
    tbl$full <- full
    tbl$main <- main
    excluded_table <- bind_rows(excluded_table, tbl)

}

excluded_table$panel <- factor(excluded_table$panel, levels=panels)
excluded_table <- excluded_table %>% arrange(panel, abbrev, N)

write.csv(excluded_table, file=outfile, row.names=F)


save.image(".excluded.rdebug")
}
