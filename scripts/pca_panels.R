require(ggplot2)
library(gridExtra)
global_theme = theme(text=element_text(size=7), 
                     plot.title=element_text(size=10, face="bold", hjust=-.1),
                     axis.title=element_text(size=7),
                     #axis.text=element_text(size=rel(.8)),
                     panel.margin=unit(c(0,rep(0, 3)), "inches"),
                     plot.margin=unit(c(0,rep(0, 3)), "inches")#unit(rep(0, 4), "inches") +

                     )
panels<-c("africa3",
"medi4",
"europe3",
"centralasia3",
"eastasia2",
"seasiaB")

titles <- c("A: Africa", "B: Mediterranean", "C: Europe",
            "G: Central Asia", "H: East Asia", "I: South-East Asia")
titles <- c(titles, "D: Africa", "E: Mediterranean", "F: Europe",
            "J: Central Asia", "K: East Asia", "L: South-East Asia")
#southafrica2
#india0
DPI <- 300

layout_list <- list()
layout_list[[1]] <- c(1,1,2,2,3,3,7,7)
layout_list[[2]] <- c(1,1,2,2,3,3,8,8)
layout_list[[3]] <- c(1,1,2,2,3,3,9,9)
layout_list[[4]] <- c(4,4,5,5,6,6,10,10)
layout_list[[5]] <- c(4,4,5,5,6,6,11,11)
layout_list[[6]] <- c(4,4,5,5,6,6,12,12)
layout_list[[1]] <- c(1,1,2,2,3,3)
layout_list[[2]] <- c(1,1,2,2,3,3)
layout_list[[3]] <- c(7,7,8,8,9,9)
layout_list[[4]] <- c(4,4,5,5,6,6)
layout_list[[5]] <- c(4,4,5,5,6,6)
layout_list[[6]] <- c(10,10,11,11,12,12)
layout_matrix <- do.call(rbind, layout_list)

fnames <-  sprintf("figures/pca/pc2d_%s.rds", panels)
fnames_pve <-  sprintf("figures/pca/pve_%s.rds", panels)
plots <- lapply(fnames, readRDS)
plots <- lapply(1:length(plots), function(i)plots[[i]]+ggtitle(titles[i]) + global_theme)
pve <- lapply(fnames_pve, readRDS)
pve <- lapply(1:length(pve), function(i)pve[[i]]+ggtitle(titles[i+6]) + 
              global_theme + xlab("PC") + 
              theme(axis.text=element_text(size=5)) +
              scale_y_continuous(labels=function(i)sprintf("%.3f", i),
                                 breaks=function(r) seq(r[1], r[2], length.out=4))
          )
plots[[1]] <- plots[[1]] + scale_x_reverse()
plots[[2]] <- plots[[2]] + scale_x_reverse()+ scale_y_reverse()
plots[[4]] <- plots[[4]] + scale_y_reverse()

plots <- c(plots, pve)
plots$ncol=8
plots$layout_matrix = layout_matrix
plots$padding = unit(0, 'line')
png("figures/paper/pca2.png", width=7*DPI, res=DPI, height=7/4*3*DPI)
do.call(grid.arrange, plots)
dev.off()

#plots <- lapply(1:length(plots), function(i)plots[[i]]+ggtitle(titles[i]) + global_theme)
#plots$ncol=3
#png("pca_test.png", width=7*DPI, res=DPI, height=7/3*2*DPI)
#do.call(grid.arrange, plots)
#dev.off()
