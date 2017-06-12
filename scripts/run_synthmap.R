source("scripts/load_pop_meta.R") #load raw
source("scripts/config.R")
source("scripts/synthmap.R")

WIDTH=7
HEIGHT=7

nruns <- as.numeric(snakemake@wildcards$nruns)
name <- snakemake@wildcards$name

pop_display <- snakemake@input$pop_display
pop_geo <- snakemake@input$pop_geo
indiv_label <- snakemake@input$indiv_label

poly_file <- snakemake@input$polygon
boundary <- read.table(poly_file)
bbox <- c(left=min(boundary[1]), right=max(boundary[1]),
      bottom=min(boundary[2]), top=max(boundary[2]))
bbox['top'] <- pmin(bbox['top'], 83)
xlim_map <- c(bbox[c('left', 'right')])
ylim_map <- c(bbox[c('bottom', 'top')])

C <- get_config(snakemake, 'synthmap')
RES <- C$RES


null_theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                            axis.title.x=element_blank(),
                                            axis.title.y=element_blank(),
#                                            legend.position="right",
#                                            legend.position="none",
#                                            legend.key.width=unit(1, "in"),
                                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                                              panel.grid.minor=element_blank(),plot.background=element_blank())

for(PC in C$pcs){
    print(sprintf("plotting synthetic map for PC %s", PC))
    m = spatial_subset_plot(subset=name, PC=PC, idp=C$idp)
    m2 <- m + null_theme +
	    coord_map("mollweide",orientation=C$fancy_proj_pars,
		      xlim=xlim_map, ylim=ylim_map) + xlim(-50, 195)+ ylim(-60, 80)
    ggsave(sprintf("figures/pca/synthmap/%s_%s.png", name, PC), m2,
       width=C$width, height=C$height)
}


