source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/ggeems_main.R")
source("scripts/config.R")


WIDTH=20
HEIGHT=20

wildcards <- snakemake@wildcards

nruns <- as.numeric(wildcards$nruns)
name <- wildcards$name

mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)

pop_display <- snakemake@input$pop_display
pop_geo <- snakemake@input$pop_geo
indiv_lael <- snakemake@input$indiv_label

RES <- snakemake@params$RES
ZOOM <- snakemake@params$ZOOM

C <- get_config(snakemake, 'map')
WIDTH <- C$width
HEIGHT <- C$height

fancy <- C$fancy
interior <- C$interior


null_theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),legend.position="bottom",
                                            legend.key.width=unit(1, "in"),
                                                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                                      panel.grid.minor=element_blank(),plot.background=element_blank())

g <- read.output.graph(mcmcpath[1])

m = make_map(mcmcpath, ZOOM, is.mrates=T, fancy_proj=fancy, just_map=T,
             interior=interior)
dummy_df <- data.frame(xmin=-360, xmax=360, ymin=-360, ymax=360)
dummy_df <- expand.grid(x=seq(-180,360,1), y=seq(-90,90, .5))
#m = m + geom_tile(data=dummy_df, aes(x=x, y=y), color=NA,
#                  inherit.aes = FALSE, fill='white', alpha=0.6)
m3 = m + ggadd.graph(g, color="#40404080") + ggadd.pts(g, color="#444444")+ null_theme
saveRDS(m3,sprintf("eemsout_gg/%s_nruns%s-map02.rds", name, nruns))
ggsave(sprintf("eemsout_gg/%s_nruns%s-map02.png", name, nruns), m3 ,
       width=WIDTH, height=HEIGHT)
#ggsave(sprintf("eemsout_gg/%s_nruns%s-map02.pdf", name, nruns), m3,
#       width=WIDTH, height=HEIGHT)

m2 = gg_add_samples_true(m, pop_geo, pop_display)
ggsave(sprintf("eemsout_gg/%s_nruns%s-map01.png", name, nruns), m2,
       width=WIDTH, height=HEIGHT)
#ggsave(sprintf("eemsout_gg/%s_nruns%s-map01.pdf", name, nruns), m2,
#       width=WIDTH, height=HEIGHT)

