suppressPackageStartupMessages({
source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/error.R")
source("scripts/config.R")
})
#save.image(".rdebug")

CC <- get_config(snakemake, plotname='error')

dist <- read.csv(snakemake@input$dist)
inddist <- read.csv(snakemake@input$inddist)
grid <- read.csv(snakemake@input$grid)
pd <- read.csv(snakemake@input$pop_display)
pg <- read.csv(snakemake@input$popgrid)
im <- read.csv(snakemake@input$ind_meta) %>%
    select(sampleId, popId)
nmax <- CC$nmax


pd <- annotate(pd)

dist_err <- get_marginal(dist, pd) 
P_dist_err <- plot_error(dist_err, CC$label,
                         CC$nmax)

grid_err <- get_marginal_grid(grid, pg, pd)
P_grid_err <- plot_error(grid_err, 'labels', CC$nmax)

worst_errors <- get_worst_errors(dist, pd)
P_worst_err <- plot_error(worst_errors, 'label', CC$nmax)

ind_err <- get_marginal_ind(inddist, im, pd)
P_ind_err <- plot_error(ind_err, "sampleId", CC$nmax)
#P_ind_err <- plot_error(ind_err, "pop", CC$nmax)
worst_ind_errors <- get_worst_errors_ind(inddist, im, pd)
P_worst_ind_err <- plot_error(worst_ind_errors, 'label', CC$nmax)

ggsave(snakemake@output$err_pop, P_dist_err, width=CC$width, height=CC$height)
ggsave(snakemake@output$err_grid, P_grid_err, width=CC$width, height=CC$height)
ggsave(snakemake@output$err_worst, P_worst_err, width=CC$width, height=CC$height)
ggsave(snakemake@output$err_ind, P_ind_err, width=CC$width, height=CC$height)
ggsave(snakemake@output$err_worst_ind, P_worst_ind_err, width=CC$width, height=CC$height)
saveRDS(P_dist_err,snakemake@output$err_pop_rds)
saveRDS(P_grid_err,snakemake@output$err_grid_rds)
saveRDS(P_worst_err,snakemake@output$err_worst_rds)
