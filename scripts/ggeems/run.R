source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/ggeems_main.R")

WIDTH=7
HEIGHT=7

args <- commandArgs(T)
nruns <- as.integer(args[1])
name <- args[2]

mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)

pop_display <- args[3]
pop_geo <- args[4]
indiv_label <- args[5]

RES <- as.integer(args[6])
ZOOM <- as.integer(args[7])
fancy <- as.integer(args[8])

null_theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),legend.position="bottom",
                                            legend.key.width=unit(1, "in"),
                                                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                                      panel.grid.minor=element_blank(),plot.background=element_blank())

g <- read.output.graph(mcmcpath[1])

m = make_map(mcmcpath, ZOOM, is.mrates=T, fancy_proj=fancy)
m2 = gg_add_samples_true(m, pop_geo, pop_display)
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates01.png", name, nruns), m2,
       width=WIDTH, height=HEIGHT)
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates01.pdf", name, nruns), m2,
       width=WIDTH, height=HEIGHT)
m3 = m + ggadd.graph(g) + ggadd.pts(g)+ null_theme
saveRDS(m3,sprintf("eemsout_gg/%s_nruns%s-mrates02.rds", name, nruns))
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates02.png", name, nruns), m3 ,
       width=WIDTH, height=HEIGHT)
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates02.pdf", name, nruns), m3,
       width=WIDTH, height=HEIGHT)


m = make_map(mcmcpath, ZOOM, is.mrates=F, fancy_proj=fancy)
m2 = gg_add_samples_true(m, pop_geo, pop_display)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates01.png", name, nruns), m2,
       width=WIDTH, height=HEIGHT)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates01.pdf", name, nruns), m2,
       width=WIDTH, height=HEIGHT)
m3 = m + ggadd.graph(g) + ggadd.pts(g)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates02.png", name, nruns), m3,
       width=WIDTH, height=HEIGHT)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates02.pdf", name, nruns), m3,
       width=WIDTH, height=HEIGHT)

