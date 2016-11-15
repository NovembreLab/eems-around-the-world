suppressPackageStartupMessages({
library(plyr)
library(ggplot2)
library(dplyr)
source("scripts/load_pop_meta.R")
source("scripts/ggeems/scatter.R")
})
#! called from snakefiles/pca.snake:make_pc_plots

get_err_mat <- function(mcmcpath){
    ipmap <- read.table(paste0(mcmcpath[1], "/ipmap.txt"))[,1]
    n_pops <- length(unique(ipmap))
    n_reps <- length(mcmcpath)
    JtDhatJ <- matrix(0,n_pops,n_pops)
    for (path in mcmcpath) {
        JtDhatJ <- JtDhatJ + 
            as.matrix(read.table(sprintf('%s/rdistJtDhatJ.txt',path)),header=F)
    }
    JtDhatJ <- JtDhatJ / n_reps
    JtDobsJ <- as.matrix(read.table(sprintf('%s/rdistJtDobsJ.txt',path)),header=F)
    colnames(JtDobsJ) <- NULL
    colnames(JtDhatJ) <- NULL

    JtDobsJ <- t(JtDobsJ - diag(JtDobsJ)/2) - diag(JtDobsJ)/2
    JtDhatJ <- t(JtDhatJ - diag(JtDhatJ)/2) - diag(JtDhatJ)/2
    
    demes <- as.matrix(read.table(sprintf('%s/demes.txt',path)),header=F)
    obs.demes <- demes[1:n_pops,]
    dist.geo <- rdist.earth(obs.demes)

    err <- melt(dist.geo, value.name="dist") %>%
	left_join( melt(JtDobsJ, value.name="Bobs") ) %>%
	left_join( melt(JtDhatJ, value.name="Bhat") ) %>%
	filter(Var1 > Var2)

}

get_pc_dist <- function(m ,max_pc=10, annotation="popId"){
    pcs <- m %>% dplyr::select(starts_with("PC"))
    max_pc <- pmin(max_pc, nrow(pcs))
    pcs <- pcs[,1:max_pc]
    dpc <- as.matrix(dist(pcs))
    rownames(dpc) <- m[,annotation]
    colnames(dpc) <- m[,annotation]
    dpc <- melt(dpc)
    pcdists <- dpc %>% group_by(Var1, Var2) %>% 
        summarize(pcdist=mean(value)) %>%
        filter(as.numeric(Var1) > as.numeric(Var2))

}
get_pc_dist_diagnorm <- function(m ,max_pc=10, annotation="popId"){
    pcs <- m %>% dplyr::select(starts_with("PC"))
    max_pc <- pmin(max_pc, nrow(pcs))
    pcs <- pcs[,1:max_pc]
    dpc <- as.matrix(dist(pcs))
    #rownames(dpc) <- m$popId
    #colnames(dpc) <- m$popId
    rownames(dpc) <- m[,annotation]
    colnames(dpc) <- m[,annotation]
    dpc <- melt(dpc)
    pcdists <- dpc %>% dplyr::group_by(Var1, Var2) %>% 
        dplyr::summarize(pcdist=mean(value))
    diag <- pcdists %>% dplyr::filter(Var1 == Var2) 
    pcdists.out <- pcdists %>% 
	ungroup() %>%
	left_join(diag, by="Var2") %>% 
	left_join(diag, by=c("Var1.x"="Var1")) %>%
	dplyr::mutate(pcdist=pcdist.x - pcdist.y/2 - pcdist /2) %>%
	dplyr::select(Var1=Var1.y, Var2=Var2.y, pcdist)  

    pcdists.out %>% filter(Var1!=Var2)
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
    out_rsq <- snakemake@output$rsq

    save.image('.Rsnakemakedebug')

    data <- load_pca_data(pc, fam, indiv_meta, pop_display)
    data <- data %>% dplyr::select(-order) %>% left_join(read.csv(pop_order))
    data <- left_join(data, read.csv(pop_geo))


    col_list <- data %>% group_by(abbrev, popId) %>% 
        summarize(color=first(color), order=mean(order)) %>% 
        arrange(order)
    col_list$color <- as.character(col_list$color)
    data$abbrev <- factor(data$abbrev, levels=col_list$abbrev)


    #set up color
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
    err[,1:2] <- err[,5:6]
    i2 <- get_grid_info(mcmcpath, indiv_meta, pop_display)
    idgrid <- i2 %>% dplyr::select(popId, grid) %>% unique()

    data <- data %>% left_join(idgrid)

    pcd.pop <- get_pc_dist_diagnorm(data, npcs)

    data.pop <- inner_join(err, pcd.pop) %>%
	left_join(idgrid, by=c("popId.x"="popId")) %>% 
        left_join(idgrid, by=c("popId.y"="popId"))


    pcplot <- plot_vs_pc(data.pop, n=npcs)
    ggsave(output, pcplot)

    pcd.grid <- get_pc_dist_diagnorm(data, npcs, annotation="grid")
    err.grid <- get_err_mat(mcmcpath)
    data.grid <- inner_join(err.grid, pcd.grid) 
    data.grid$is_outlier <- F

    #data2 <- data %>% group_by(grid.x, grid.y) %>% 
    #    summarize(Bobs=mean(Bobs), pcdist=mean(pcdist), is_outlier=any(is_outlier))
    pcplot <- plot_vs_pc(data.grid, n=npcs)
    ggsave(output2, pcplot)


    save.image('.Rsnakemakedebug')
    print("PCGIR")
    pcd.all.grid <- lapply(1:npcs, function(i){
                       get_pc_dist_diagnorm(data, i, annotation="grid");})
    pcd.all.grid <- join_all(pcd.all.grid, by=c("Var1", "Var2"), type='inner')
    names(pcd.all.grid)[1:npcs+2] <- sprintf("pcdist%s", 1:npcs)
    pcd.all.grid <- pcd.all.grid %>% 
        filter(Var1 > Var2) %>% 
        left_join(data.grid)
    grid.rsqs <- sapply(1:npcs, function(i){
                s <- sprintf("pcdist%s", i)
                summary(lm(pcd.all.grid[,s]~ pcd.all.grid$Bobs))$adj.r.squared})
    s <- 'Bhat'
    eems.rsq <- summary(lm(pcd.all.grid[,s]~ pcd.all.grid$Bobs))$adj.r.squared
    pc_str <- sprintf("%s", 1:npcs)
    pc_str <- factor(pc_str, levels=pc_str)
    df <- data.frame(x=pc_str, y=grid.rsqs)
    P <- ggplot(df) + geom_bar(aes(x=x, y=y), stat="identity", fill='lightgray') +       
        geom_hline(aes(yintercept=eems.rsq), color='red') +
        theme_classic() + 
        xlab("Number of PCs") +
        ylab("Correlation between model and data")
#         theme(axis.text.x = element_text(size=rel(.4), angle = 90, hjust = 1))
    ggsave(out_rsq, P, width=7, height=3)


} 








