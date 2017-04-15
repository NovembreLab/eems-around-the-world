suppressPackageStartupMessages({
	library(ggplot2)
	library(dplyr)
	source("scripts/config.R")
	source("scripts/ggpca2d.R")
        source("scripts/themes.R")
})
C <- get_config(snakemake, 'pve')

pve <- read.table(snakemake@input$pve_file)[1:C$nmax,1]
df <- data.frame(PC=1:length(pve), pve=pve) 
G <- ggplot(df, aes(y=pve, x=as.factor(PC))) + geom_bar(stat="identity")  +
        pve_theme(base_size=C$theme_size)+ xlab("PC")
    ggsave(snakemake@output$png, G, width=C$width, height=C$height)
    saveRDS(G, snakemake@output$rds)

