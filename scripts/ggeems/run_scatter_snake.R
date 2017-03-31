source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/scatter.R")
args <- commandArgs(T)
nruns <- as.integer(snakemake@wildcards$nruns)
name <- snakemake@wildcards$name

mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)

pop_display <- snakemake@input$pop_display
pop_geo_file <- snakemake@input$pop_geo
indiv_label <- snakemake@input$indiv_label

diffs <- snakemake@input$diffs
order <- snakemake@input$order

exfam <- snakemake@input$exfam
exname <- snakemake@input$exname

print(exfam)


outnames <- c(snakemake@output$p1,
		snakemake@output$p2,
		snakemake@output$p3,
		snakemake@output$p4,
		snakemake@output$p5,
		snakemake@output$p6,
		snakemake@output$p7)

ggscatter(mcmcpath, diffs, order, pop_display, pop_geo_file, indiv_label, outnames, exfam)
