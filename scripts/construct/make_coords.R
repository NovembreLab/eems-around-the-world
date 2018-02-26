require(dplyr)
require(fields)
fam <- read.table(snakemake@input$fam)
fam <- data.frame(sampleId=fam[,1])
indiv_full <- read.csv(snakemake@input$indiv_full)

fam %>% left_join(indiv_full) %>% select(longitude, latitude) %>%
    as.matrix -> mat
mat %>% saveRDS(snakemake@output$coord_rds)

fields::rdist.earth(mat) %>% saveRDS(snakemake@output$dist_rds)

