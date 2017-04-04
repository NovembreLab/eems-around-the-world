source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/scatter2.R")
pop_display <- read.csv(snakemake@input$pop_display)
dist <- read.csv(snakemake@input$dist)
grid <- read.csv(snakemake@input$grid)



outnames <- c(snakemake@output$p1,
		snakemake@output$p2,
		snakemake@output$p3,
		snakemake@output$p4,
		snakemake@output$p5,
		snakemake@output$p6,
		snakemake@output$p7)

ggscatter(dist, grid, pop_display,
          outnames)
