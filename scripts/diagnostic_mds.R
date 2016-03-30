source("pw_plot.R")
diffs = snakemake@input$diffs
order = snakemake@input$order
indiv_meta = snakemake@input$indiv_meta
pop_display = snakemake@input$pop_display
pdfname=snakemake@output$pdf


q <- cmdscale(read.table(diffs))
plot_pw(q[,1], q[,2], indiv_meta, pop_display, order, pdfname)

