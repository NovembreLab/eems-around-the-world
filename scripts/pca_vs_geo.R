suppressPackageStartupMessages({
library(ggplot2)
library(dplyr)
source("scripts/load_pop_meta.R")
source("scripts/ggeems/scatter.R")
})
#! called from snakefiles/pca.snake:make_pc_plots

get_pc_dist <- function(m ,max_pc=10){
    pcs <- m %>% dplyr::select(starts_with("PC"))
    max_pc <- pmin(max_pc, nrow(pcs))
    pcs <- pcs[,1:max_pc]
    dpc <- as.matrix(dist(pcs))
    rownames(dpc) <- m$popId
    colnames(dpc) <- m$popId
    dpc <- melt(dpc)
    pcdists <- dpc %>% group_by(Var1, Var2) %>% 
        summarize(pcdist=mean(value)) %>%
        filter(as.numeric(Var1) > as.numeric(Var2))

}

get_sample_dist <- function(){
    diffs <- as.matrix(read.table("eems/world.diffs"))
    order <- read.table("eems/world.order", as.is=T)
    order <- data.frame(sampleId=order[,1])
    order <- order %>% left_join(m) %>% dplyr::select(popId)
    rownames(diffs) <- order[,1]
    colnames(diffs) <- order[,1]
    diffs <- melt(diffs)
    gendists  <- diffs %>% group_by(Var1, Var2) %>% 
        summarize(gendist=mean(value))  %>%
        filter(as.numeric(Var1) > as.numeric(Var2))
}


if(exists('snakemake')){
    pc <- snakemake@input[['pc']]
    fam <- snakemake@input[['fam']]
    indiv_meta <- snakemake@input[['indiv_meta']]
    pop_display <- snakemake@input[['pop_display']]
    pop_order <- snakemake@input[['pop_order']]
    pop_geo <- snakemake@input[['pop_geo']]
    npcs <- as.numeric(snakemake@wildcards$npcs)

    diffs <- snakemake@input$diffs
    order <- snakemake@input$order


    mcmcpath <- sprintf('eemsout/0/%s/', snakemake@wildcards$name)

    output <- snakemake@output[['pcvsdist']]
    output2 <- snakemake@output[['pcvsgrid']]




    data <- load_pca_data(pc, fam, indiv_meta, pop_display)
    data <- data %>% dplyr::select(-order) %>% left_join(read.csv(pop_order))
    data <- left_join(data, read.csv(pop_geo))


    col_list <- data %>% group_by(abbrev, popId) %>% 
        summarize(color=first(color), order=mean(order)) %>% 
        arrange(order)
    col_list$color <- as.character(col_list$color)
    data$abbrev <- factor(data$abbrev, levels=col_list$abbrev)

	# if we have a set of excluded guys
	if(!is.null(snakemake@input$exfam)){
		exfam <- read.table(snakemake@input$exfam)
		excluded <- data$sampleId %>% setdiff(exfam[,1])
		expops <- data %>% filter(sampleId %in% excluded) %>% 
            dplyr::select(popId) %>% unique()  %>% unlist() %>% c()
		col_list[col_list$popId %in% expops,'color'] <- 'red'
	}  else{

        cv <- as.character(col_list$color)
        names(cv) <- col_list$abbrev
        col <- list(scale_color_manual(values=cv),
                    scale_fill_manual(values=cv))
    }
    print("loading err")
    err <- get_pop_mats(mcmcpath, diffs, order, pop_display,
                        indiv_meta, pop_geo)$pw
    print(head(err))
    err[,1:2] <- err[,5:6]

    pcd <- get_pc_dist(data, npcs)
    data <- inner_join(err, pcd)
    i2 <- get_grid_info(mcmcpath, indiv_meta, pop_display)
    idgrid <- i2 %>% dplyr::select(popId, grid) %>% unique()
    data <- data %>% left_join(idgrid, by=c("popId.x"="popId")) %>% 
        left_join(idgrid, by=c("popId.y"="popId"))



    pcplot <- plot_vs_pc(data, n=npcs)
    ggsave(output, pcplot)

    data2 <- data %>% group_by(grid.x, grid.y) %>% 
        summarize(Bobs=mean(Bobs), pcdist=mean(pcdist), is_outlier=any(is_outlier))
    pcplot <- plot_vs_pc(data2, n=npcs)
    ggsave(output2, pcplot)
    save.image('.Rsnakemakedebug')
} 








