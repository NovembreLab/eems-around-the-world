suppressPackageStartupMessages({
	library(ggplot2)
})
#global_theme from pca_panels.R, currently unused
global_theme = theme(text=element_text(size=7), 
                     plot.title=element_text(size=10, face="bold", hjust=-.1),
                     axis.title=element_text(size=7),
                     #axis.text=element_text(size=rel(.8)),
                     panel.spacing=unit(c(0,rep(0, 3)), "inches"),
                     plot.margin=unit(c(0,rep(0, 3)), "inches")#unit(rep(0, 4), "inches") +
                     )

#PVE-plot: theme_classic
pve_theme <- function(...)theme_classic(...) 

pca_2d_theme <- function(...){
    list(
    theme_classic(...),
    scale_color_identity(),
    scale_fill_identity() , 
    theme(legend.position='none',
          plot.margin = margin(1, 1, 1, 1, 'mm'),
          axis.title = element_text(size=rel(.8), margin(.1, .1, .1, .1, 'mm')),
          axis.text = element_text(size=rel(.8)),
          axis.line = element_line(size=unit(.1, 'mm')),
          axis.ticks = element_line(size=rel(.8))
          )
    )
}


map_inlet_theme <- function(...){
	theme_classic(...) +
	theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
	    axis.text.y=element_blank(),
            axis.ticks=element_blank(),
	    axis.title.x=element_blank(),
	    axis.title.y=element_blank(),
	    panel.background=element_blank(),
	    panel.border=element_blank(),
	    panel.grid.major=element_blank(),
	    panel.grid.minor=element_blank(),
	    plot.background=element_blank(),
            legend.position="none")
}
