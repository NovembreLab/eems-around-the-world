source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/ggeems_main.R")

W=11 * 1.5
H=8 * 1.5 

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


g <- read.output.graph(mcmcpath[1])

m = make_map(mcmcpath, ZOOM, is.mrates=T, fancy_proj=fancy)
m2 = gg_add_samples_true(m, pop_geo, pop_display)
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates01.png", name, nruns), m2,
       width=W, height=H)
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates01.pdf", name, nruns), m2,
       width=W, height=H)
m3 = m + ggadd.graph(g) + ggadd.pts(g)
saveRDS(m3, sprintf("eemsout_gg/%s_nruns%s-mrates02.rds", name, nruns))
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates02.png", name, nruns), m3,
       width=W, height=H)
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates02.pdf", name, nruns), m3,
       width=W, height=H)


m = make_map(mcmcpath, ZOOM, is.mrates=F, fancy_proj=fancy)
m2 = gg_add_samples_true(m, pop_geo, pop_display)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates01.png", name, nruns), m2,
       width=W, height=H)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates01.pdf", name, nruns), m2,
       width=W, height=H)
m3 = m + ggadd.graph(g) + ggadd.pts(g)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates02.png", name, nruns), m3,
       width=W, height=H)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates02.pdf", name, nruns), m3,
       width=W, height=H)

