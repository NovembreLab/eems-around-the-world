source("pw_plot.R")
pc = snakemake@input$pc
order = snakemake@input$order
indiv_meta = snakemake@input$indiv_meta
pop_display = snakemake@input$pop_display
pdfname=snakemake@output$pdf

q <- read.table(pc)
plot_pw(q[,1], q[,2], indiv_meta, pop_display, order, pdfname)

