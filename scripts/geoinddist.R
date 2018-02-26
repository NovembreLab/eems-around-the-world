suppressPackageStartupMessages({
library(dplyr)
library(reshape2)
require(fields)
source("scripts/load_pop_meta.R")
})

geo_file <- snakemake@input$geodist
im_file <- snakemake@input$indiv_meta
outname <- snakemake@output[[1]]


geo <- read.csv(geo_file)
im <- read.csv(im_file)

im.x <- im %>% select(popId.x=popId, sampleId.x=sampleId)
im.y <- im %>% select(popId.y=popId, sampleId.y=sampleId)
save.image("QQ")

geo2 <- geo %>% left_join(im.x)  %>%
    left_join(im.y) #
geo2$popId.x <- as.character(geo2$popId.x)
geo2$popId.y <- as.character(geo2$popId.y)
geo2$sampleId.x <- as.character(geo2$sampleId.x)
geo2$sampleId.y <- as.character(geo2$sampleId.y)
geo2_same <- geo2 %>% filter(popId.x==popId.y) %>%
    filter(sampleId.x < sampleId.y)


geo3 <- bind_rows(
                  geo2 %>% filter(popId.x!=popId.y),
                  geo2_same) %>% select(-popId.x, -popId.y)


write.csv(geo3, outname, row.names=F)

