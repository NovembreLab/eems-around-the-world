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
                    c(1,1,1,2),
                    c(1,1,1,3),
                    c(1,1,1,4),
                    c(5,6,7,8),
                    c(5,6,7,9)
#                    c(1,1,1,1,1,3,3),
#                    c(1,1,1,1,1,4,4)
                    )

null_theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),legend.position="bottom",
                                            legend.key.width=unit(1, "in"),
                                                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                                      panel.grid.minor=element_blank(),plot.background=element_blank())

world_map <- readRDS(sprintf("eemsout_gg/%s_nruns4-mrates02.rds", PANEL))
#world_map <- readRDS("test.rds")
pc1 <- readRDS(sprintf("figures/pca/2d/%s_pc1.rds", PANEL)) + global_theme
pc2 <- readRDS(sprintf("figures/pca/2d/%s_pc3.rds", PANEL)) + global_theme
pve <- readRDS(sprintf("figures/pca/pve/%s.rds", PANEL)) + global_theme
map <- readRDS(sprintf("figures/paper/map_%s.rds", PANEL)) + global_theme
#map <- map + coord_map("mollweide",orientation=c(90,10, 40)) + xlim(-20, 195)

l <- readRDS(sprintf("figures/dists/%s.rds", PANEL))

wmap = world_map + null_theme + global_theme + 
    scale_size_continuous(guide='none', range=c(.2,2)) +
    theme(plot.title=element_text(size=10, face="bold", hjust=0))

png(sprintf("figures/paper/%s.png", PANEL), 
    width=7*500, height=5.2*500, res=500)
#pdf("test.pdf", width=7, height=3)
g <-grid.arrange(wmap + ggtitle("A"), 
		 map + ggtitle("B"), 
		 pc1 + ggtitle("C"), 
		 pc2 + ggtitle("D"),
		 l +ggtitle("F"), 
                 l + ggtitle("G"),
                 l + ggtitle("H"),
                 #l[[4]] + ggtitle("I"),
		 pve + ggtitle("I"),
                 l + ggtitle("J"),
                 layout_matrix=layout_mat, 
		 heights=list(0.8,1.2,1.2,.9,.9),
		 widths=c(1,1,1,1)) 
dev.off()


