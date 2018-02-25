source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/ggeems_main.R")
source("scripts/config.R")
source("scripts/annotation.R")


WIDTH=7
HEIGHT=7

nruns <- as.numeric(snakemake@wildcards$nruns)
name <- snakemake@wildcards$name

eems_opt <- snakemake@config$eems[[name]]
mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)
if("continue" %in% names(eems_opt)){
    n2 <- eems_opt[['continue']]
    mcmcpath2 <- sprintf('eemsout/%d/%s/', 0:(nruns-1), n2)
    mcmcpath <- c(mcmcpath, mcmcpath2)
    print(mcmcpath)
}

pop_display <- snakemake@input$pop_display
pop_geo <- snakemake@input$pop_geo
indiv_label <- snakemake@input$indiv_label

C <- get_config(snakemake, 'eemsvar')
RES <- C$RES
print(C)

alpha_limits <- c(C$min_alpha, C$max_alpha)
alpha_null <- ifelse(C$fancy, 0.9, 0.6)


null_theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
                                            #legend.position="left",
                                            legend.position="none",
                                            legend.key.width=unit(.2, "in"),
                                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                              panel.grid.minor=element_blank(),plot.background=element_blank())

g <- read.output.graph(mcmcpath[1])

m = make_map(mcmcpath, C$zoom, is.mrates=T, fancy_proj=C$fancy, 
     alpha_limits=alpha_limits, fancy_proj_pars=C$fancy_proj_pars,
     signplot=F, alpha_null=alpha_null, varplot=T)
m3 <- m + null_theme
if(C$add_graph) m3 <- m3 +ggadd.graph(g)
if(C$add_pts) m3 <- m3 + ggadd.pts(g)
if(C$add_pts_color) m3 <- m3 + ggadd.pts.color(g)
if(C$add_label) m3 <- gg_add_samples_true(m3, pop_geo, pop_display)

if("annotations" %in% names(C)){
    for(anno in C$annotations){
	print(sprintf("annotating with %s", anno))
	m3 <- ggadd_annotation(m3, anno)
    }
}

saveRDS(m3,sprintf("eemsout_gg/%s_nruns%s-mrates03.rds", name, nruns))
ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates03.png", name, nruns), m3 ,
       width=C$width, height=C$height)
#ggsave(sprintf("eemsout_gg/%s_nruns%s-mrates02.pdf", name, nruns), m3,
#       width=WIDTH, height=HEIGHT)


if(C$do_q_plots){
m3 = m + ggadd.graph(g) + ggadd.pts(g)
ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates02.png", name, nruns), m3,
       width=WIDTH, height=HEIGHT)
#ggsave(sprintf("eemsout_gg/%s_nruns%s-qrates02.pdf", name, nruns), m3,
#       width=WIDTH, height=HEIGHT)
warnings()
}

