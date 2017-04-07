require(ggplot2)
require(gridExtra)
require(ggmap)

alpha_limits <- c(0.3, 1)

PANEL='global2'
if(length(commandArgs(T))>0){PANEL <- commandArgs(T)[1]}

global_theme = theme(text=element_text(size=7), 
                 plot.title=element_text(size=10, face="bold", hjust=-.2),
                 axis.title=element_text(size=7),
                 axis.line=element_blank(),
                 axis.text=element_text(size=rel(.8)),
                 panel.margin=unit(c(0,rep(0, 3)), "inches"),
                 plot.margin=unit(c(0,rep(0, 3)), "inches")#unit(rep(0, 4), "inches") +

                 )

layout_mat <- rbind(
                    c(1,1,1,2,2,2),
                    c(3,3,3,3,4,4),
                    c(5,5,6,6,7,7),
                    c(5,5,6,6,8,8)
                    )

null_theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),legend.position="bottom",
                                            legend.key.width=unit(1, "in"),
                                                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                                      panel.grid.minor=element_blank(),plot.background=element_blank())

world_map <- readRDS(sprintf("eemsout_gg/%s_nruns4-mrates02.rds", PANEL))
barrier_map <- readRDS(sprintf("eemsout_gg/%s_nruns4-mrates01.rds", PANEL))
#world_map <- readRDS("test.rds")
pc1 <- readRDS(sprintf("figures/pca/2d/%s_pc1.rds", PANEL)) + global_theme
pc2 <- readRDS(sprintf("figures/pca/2d/%s_pc3.rds", PANEL)) + global_theme
pve <- readRDS(sprintf("figures/pca/pve/%s.rds", PANEL)) + global_theme
map <- readRDS(sprintf("figures/paper/map_%s.rds", PANEL)) + global_theme
dists <- readRDS(sprintf("figures/dists/%s.rds", PANEL))
errors <- readRDS(sprintf("eemsout_gg/%s_nruns4-error-pop01.rds", PANEL))
#map <- map + coord_map("mollweide",orientation=c(90,10, 40)) + xlim(-20, 195)

wmap = world_map + null_theme + global_theme + 
    scale_size_continuous(guide='none', range=c(.2,2)) +
    theme(
          legend.position="none")
bmap = barrier_map + null_theme + global_theme + 
    scale_size_continuous(guide='none', range=c(.2,2)) +
    theme(
          legend.position="none")

png(sprintf("figures/paper/%s.png", PANEL), 
    width=7*500, height=5.2*500, res=500)
#pdf("test.pdf", width=7, height=3)
g <-grid.arrange(wmap,
                 bmap,
                 dists,
                 errors,
                 pc1,
                 pc2,
                 pve,
                 map,
                 layout_matrix=layout_mat, 
		 heights=list(2,2,1,1),
		 widths=c(1,1,1,1, 1, 1) * 1.2) 
dev.off()


