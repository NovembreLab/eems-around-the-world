library(ggplot2)
library(MASS)
library(dplyr)
library(fields)
library(reshape2)
library(Cairo)
source("scripts/eems_plot/load_output.r")


library(gridExtra)

layout_mat=cbind(c(1,1), c(2,2), c(3,3), c(4,5))

FOR_PAPER=T
EXPLOR=!FOR_PAPER

if(FOR_PAPER){
    global_theme = theme(text=element_text(size=7), 
                         plot.title=element_text(size=10, face="bold", hjust=-.3),
                         axis.title=element_text(size=7),
                         axis.text=element_text(size=rel(.8)),
                         panel.margin=unit(c(0,rep(0, 3)), "inches"),
                         plot.margin=unit(c(0,rep(0, 3)), "inches")#unit(rep(0, 4), "inches") +

                         )
    PT_SIZE=0.3
    out_width=7/4
    out_h1=7/4
    out_h2=3.5/4
} else {
    global_theme = list()
    PT_SIZE=1
}


get_grid_info <- function(mcmcpath, indiv_label_file, pop_display_file){
    pop_display <- read.csv(pop_display_file)
    o <- read.table(sprintf("%s/ipmap.txt", mcmcpath[1]))
    names(o) <- 'grid'
    o <- cbind(grid=o, grid_order=1:nrow(o))
    indiv_label <- read.csv(indiv_label_file)     
    i2 <- bind_cols(indiv_label,grid=o) %>% left_join(pop_display)
}

get_fit_matrix_full <- function(i2){
    x <- i2 %>% group_by(grid) %>% 
        summarize(grid_order=first(grid_order), f=first(popId), a=first(name)) %>% 
        arrange(grid_order) %>% dplyr::select(f, a) %>% 
        mutate(f=paste(as.character(a), as.character(f), sep="_"))
    return(x$f)
}
get_fit_matrix_abbrev <- function(i2){
    x <- i2 %>% group_by(grid) %>% 
        summarize(grid_order=first(grid_order), f=paste(unique(abbrev), collapse="|"), a=first(name)) %>% 
#        summarize(grid_order=first(grid_order), f=first(abbrev), a=first(name)) %>% 
        arrange(grid_order) %>% dplyr::select(f, a)
    return(x$f)
}
get_fit_matrix_ids <- function(i2){
    x <- i2 %>% group_by(grid) %>% 
        summarize(grid_order=first(grid_order), f=paste(unique(popId), collapse="|"), a=first(name)) %>% 
        #summarize(grid_order=first(grid_order), f=first(popId), a=first(name)) %>% 
        arrange(grid_order) %>% dplyr::select(f, a)
    return(x$f)
}

plot_within <- function(within){
    ll <- lm(Wobs ~ What, data=within)
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    P <- ggplot(within) + geom_text(aes(x=Wobs, y=What, label=label,
                                        color=is_outlier)) +
        cm  + theme_classic() + 
        geom_abline(intercept=0) + 
        theme(legend.position=0) +
        xlab("Observed within-population dissimilarity") +
        ylab("Fitted within-population dissimilarity") +
	labs(title = paste("r² = ",signif(summary(ll)$adj.r.squared, 3))) + 
	theme(plot.title = element_text(size = rel(.5), hjust=0))
}

plot_pw <- function(df){
    ll <- lm(Bobs ~ Bhat, data=df)
        r2 <- paste("r² = ",signif(summary(ll)$adj.r.squared, 2))
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    P <- ggplot(df) + geom_point(aes(y=Bobs, x=Bhat, 
                  color=is_outlier), size=PT_SIZE)  +
        theme_classic() + 
        geom_abline(intercept=0) +
        theme(legend.position=0) +
        ylab("Genetic dissimilarity") +
        xlab("Fitted dissimilarity") + cm 
    if(FOR_PAPER) P <- P+ 
        labs(title = r2) + 
        theme(plot.title = element_text(size = rel(.5), hjust=0)) +
        annotate("text", Inf, -Inf, label = r2, hjust = 1, vjust = -0.3, size=2)
    return(P)
}
plot_vs_pc <- function(df, n=1){
    if(n==1){pclab <- 'Dissimilarity based on PC1'}
    else{
	pclab <- sprintf("Dissimilarity based on PC1-%s", n)
    }
    ll <- lm(Bobs ~ pcdist, data=df)
        r2 <- paste("r² = ",signif(summary(ll)$adj.r.squared, 2))
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    P <- ggplot(df) + geom_point(aes(x=pcdist, y=Bobs, 
                  color=is_outlier), size=PT_SIZE)  +
        theme_classic() + 
        geom_abline(intercept=ll$coefficients[1], slope=ll$coefficients[2]) +
        theme(legend.position=0) +
        ylab("Genetic dissimilarity") +
        xlab(pclab) + cm + 
	labs(title = paste("r² = ",signif(summary(ll)$adj.r.squared, 3))) + 
	theme(plot.title = element_text(size = rel(.5), hjust=0)) +
        annotate("text", Inf, -Inf, label = r2, hjust = 1, vjust = -0.3, size=2)
}

plot_vs_true <- function(df){
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    ll <- lm(Bobs ~ dist, data=df)
        r2 <- paste("r² = ",signif(summary(ll)$adj.r.squared, 2))
    P <- ggplot(df) + geom_point(aes(x=dist, y=Bobs, 
                                  color=is_outlier), size=PT_SIZE)  +
#            geom_text(aes(x=dist, y=Bobs, label=label), data=df[df$is_outlier,]) + 
        theme_classic() + 
        geom_abline(intercept=ll$coefficients[1], slope=ll$coefficients[2]) +
        theme(legend.position=0) + cm + 
        xlab("Geographic distance (km)") +
        ylab("Genetic dissimilarity") +
	labs(title = paste("r² = ",signif(summary(ll)$adj.r.squared, 3))) + 
	theme(plot.title = element_text(size = rel(.5), hjust=0)) + 
        annotate("text", Inf, -Inf, label = r2, hjust = 1, vjust = -0.3, size=2)
}
plot_median_error <- function(grid_error, nmax=50){
    grid_error <- grid_error %>% arrange(-nrmse)
    grid_error$popId <- factor(grid_error$popId, levels=grid_error$popId)
    cm <- scale_fill_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    if(is.null(nmax)){
        P <- ggplot(grid_error) + geom_bar(aes(y=nrmse, x=popId, 
                                          fill=is_outlier), stat='identity') 
    } else {
        nmax <- pmin(nmax, nrow(grid_error))
        P <- ggplot(grid_error[1:nmax,]) + geom_bar(aes(y=nrmse, x=popId, 
                                          fill=is_outlier), stat='identity') 
    }
    P <- P + theme_classic()
    P <- P + xlab("") + ylab("RMSE")
#        P <- P + ggtitle("Error by Population")
    P <- P + theme(axis.text.x = element_text(size = rel(1), angle = 90))
    P <- P + theme(legend.position=0) 
    library(scales)
    P <- P + scale_y_continuous(labels=function(i)sprintf("%.2f", i))
#    P <- P + scale_y_continuous(labels=comma_format(digits=2))
    P <- P + cm
}

ggscatter <- function(mcmcpath, diffs, order, pop_display_file, pop_geo_file, 
                      indiv_label_file, outnames,
                      exfam=NULL){



    oDemes <- read.table(sprintf("%s/rdistoDemes.txt", mcmcpath[1]))
    sizes <- oDemes[,3]
    coords <- oDemes[,1:2]
    n_pops <- length(sizes)
    n_reps <- length(mcmcpath)

    JtDobsJ <- matrix(0,n_pops,n_pops)
    JtDhatJ <- matrix(0,n_pops,n_pops)

    for (path in mcmcpath) {
        JtDobsJ <- JtDobsJ + 
            as.matrix(read.table(sprintf('%s/rdistJtDobsJ.txt',path)),header=F)
        JtDhatJ <- JtDhatJ + 
            as.matrix(read.table(sprintf('%s/rdistJtDhatJ.txt',path)),header=F)
    }
    JtDobsJ <- JtDobsJ/n_reps
    JtDobsJ[is.nan(JtDobsJ)] <- median(diag(JtDobsJ), na.rm=T) #nan fix
    JtDhatJ <- JtDhatJ/n_reps

    i2 <- get_grid_info(mcmcpath, indiv_label_file, pop_display_file)
    pop_labels <- get_fit_matrix_abbrev(i2)
    pop_ids <- get_fit_matrix_ids(i2)
    pop_labels_full<- get_fit_matrix_full(i2)
    label_mat <- outer(FUN=paste, pop_labels, pop_labels, sep="|")


    g <- read.output.graph(mcmcpath[1])

    Wobs <- diag(JtDobsJ) 
    What <- diag(JtDhatJ) 
    ones <- matrix(1,n_pops,1)                               
    Bobs <- JtDobsJ - (Wobs%*%t(ones) + ones%*%t(Wobs))/2   
    Bhat <- JtDhatJ - (What%*%t(ones) + ones%*%t(What))/2   
    dists <- rdist.earth(coords, miles=F)
    dists <- melt(dists, value.name='dist')
    colnames(Bobs) <- NULL
    colnames(Bhat) <- NULL

    Bobs <- melt(Bobs, value.name='Bobs')
    Bhat <- melt(Bhat, value.name='Bhat')
    labels <- melt(label_mat, value.name='label')


    if(is.null(exfam)){
        outlier_pop <- data.frame(popId=c(), grid=c())
    }else{
        indiv_label <- read.csv(indiv_label_file)
		exfam <- read.table(exfam)
		excluded <- i2$sampleId %>% setdiff(exfam[,1])
        outlier_pop <- i2 %>% filter(sampleId %in% excluded) %>% 
            dplyr::select(grid, popId) %>% unique()
    }


    #errors based on grid cells
    df <- inner_join(Bobs, Bhat) %>% inner_join(labels)  %>% inner_join(dists)
    df$error <- abs(df$Bobs - df$Bhat)
    df$is_outlier <- df$Var1 %in% outlier_pop$grid | 
        df$Var2 %in% outlier_pop$grid


    

    #per-grid error
    grid_error <- df %>% group_by(Var1) %>% 
        summarize(rmse=sqrt(mean(error^2)), mae=mean(abs(error)),
                  med=median(abs(error)))  %>%
        mutate(nrmse=rmse/mean(rmse)) 


    pop_labels <- paste(toupper(substr(pop_labels, 1, 2)), substr(pop_ids, 1,3), sep="-")
    grid_error <- grid_error %>% cbind(label=pop_labels, popId=pop_labels) %>% 
        arrange(-nrmse)
#    grid_error <- grid_error %>% cbind(label=pop_labels, popId=pop_ids) %>% 
#        arrange(-nrmse)

    grid_error$label <- factor(grid_error$label, levels=unique(grid_error$label))
    grid_error <- grid_error %>% mutate(is_outlier=Var1 %in% outlier_pop$grid)
    

    #df for within grid error
    within <- data.frame(label=as.character(pop_labels), Wobs=Wobs, What=What,
                         popId=pop_ids, is_outlier=F)
    within$is_outlier[outlier_pop$grid] <- T
    
    df <- df %>% filter(Var1 < Var2)

    p1 <- plot_pw(df) + global_theme +  ggtitle( "B")  
    p2 <- plot_vs_true(df) + global_theme+  ggtitle( "A") 
    p4 <- plot_median_error(grid_error, nmax=10) + global_theme+  ggtitle( "D") + 
        theme(axis.text.x=element_text(size=rel(.6)), plot.margin=unit(c(0,0,-0.4,0), "cm"))


    panel=strsplit(mcmcpath, "/")[[1]][3]
    panel <<- panel
    RDS1 <- sprintf("figures/pcvsgrid/%s_pc1-2.rds", panel)
    RDS2 <- sprintf("figures/rsq/%s_pc1-10.rds", panel)
    out_grid <- sprintf("figures/paper/scatter_%s_nruns%d.png", panel, length(mcmcpath))

    p5 <- readRDS(RDS1) + global_theme+  ggtitle( "C") 
    p6 <- readRDS(RDS2) + global_theme+  ggtitle( "E")  + 
        theme(plot.margin=unit(c(-0.5,0,0,0), "cm"), 
         plot.title=element_text(size=10, face="bold", hjust=-.3, vjust=1.4))

    DPI = 300
    png(filename=out_grid, width=7*DPI, height=1.5*DPI, res=DPI)
    grid.arrange(p2, p1, p5, p4, p6, ncol = 4, 
                              layout_matrix = layout_mat)
    dev.off()

    ggsave(outnames[1], plot_pw(df))
    ggsave(outnames[2], plot_vs_true(df))
    ggsave(outnames[3], 
           plot_within(within))
    ggsave(outnames[4], 
           plot_median_error(grid_error), width=7, height=3)
    
    l <- get_pop_mats(mcmcpath, diffs, order, 
                      pop_display_file, indiv_label_file,
                      pop_geo_file)

    l$error$is_outlier <- l$error$popId %in% outlier_pop$popId
    l$pw$is_outlier <- l$pw$popId.x %in% outlier_pop$popId |
                       l$pw$popId.y %in% outlier_pop$popId

    ggsave(outnames[5], 
           plot_median_error(l$error))
    ggsave(outnames[6], 
           plot_pw(l$pw))
    ggsave(outnames[7], 
           plot_vs_true(l$pw))
}


if(T){
    mcmcpath <- 'eemsout/3/medi4/'
    diffs <- 'eems/medi4.diffs'
    order <- 'eems/medi4.order'
    pop_display <- '../meta/pgs/gvar.pop_display'
    indiv_label_file <- 'subset/medi4.indiv_meta'
    pop_display_file <- pop_display
    pop_geo_file <- 'subset/medi4.pop_geo'
}

get_pop_mats <- function(mcmcpath, diffs, order, pop_display_file, indiv_label_file,
                         pop_geo_file){
    d <- as.matrix(read.table(diffs)       )
    ord <- as.matrix(read.table(order)       )
    odf <- data.frame(sampleId=ord[,1], v=1:nrow(ord))

    pop_geo <- read.csv(pop_geo_file)
    dmat <- rdist.earth(pop_geo[,c('longitude', 'latitude')], miles=F) 
    rownames(dmat) <- pop_geo$popId
    colnames(dmat) <- pop_geo$popId
    d3 <- melt(dmat, value.name="dist")
    names(d3) <- c('popId.x', 'popId.y', 'dist')

    i2 <- get_grid_info(mcmcpath, indiv_label_file, pop_display_file)
    idpop <- data.frame(popId=pop_geo$popId, id=1:nrow(pop_geo))
     v <- idpop$id          
     names(v) <- idpop$popId




    n <- nrow(i2)
    n_pops <- max(i2$grid)
    J <- matrix(0, nrow=n, ncol=n_pops)       
    J[cbind(1:n, i2$grid)] <- 1 #/i2$n
    ssJ <- colSums(J)
    ssJmat <- ssJ %*% t(ssJ)
    diag(ssJmat) <- diag(ssJmat) - ssJ

    K <- matrix(0, nrow=n, ncol=length(unique(i2$popId)))       
    K[cbind(1:n, v[as.character(i2$popId)])] <- 1#/i2$popn
    ssK <- colSums(K)
    ssK <- pmax(ssK, 1)
    ssKmat <- ssK %*% t(ssK)
    diag(ssKmat) <- diag(ssKmat) - ssK
    ssKmat <- pmax(ssKmat, 1)

    d1 <- (t(K) %*% d %*% K)/ssKmat
    d1[is.nan(d1)] <- 0

    JtDhatJ <- matrix(0,n_pops,n_pops)
    for (path in mcmcpath) {
        JtDhatJ <- JtDhatJ + 
            as.matrix(read.table(sprintf('%s/rdistJtDhatJ.txt',path)),header=F)
    }
    n_reps <- length(mcmcpath)
    JtDhatJ <- JtDhatJ/n_reps
    diag_hat <- diag(JtDhatJ) * ssJ * (ssJ-1)
    hat <- JtDhatJ * ssJ %o% ssJ
    diag(hat) <- diag_hat
    d2 <- (t(K) %*% ginv(t(J)) %*% hat %*% ginv(J) %*% K) / ssKmat

    #ones <- matrix(1, nrow(d2),1)                               
    #n2 <-  (diag(d2)%*%t(ones) + ones%*%t(diag(d2)))/2   
    #n1 <-  (diag(d1)%*%t(ones) + ones%*%t(diag(d1)))/2   

    d1 <- melt(d1, value.name="Bobs")
    d2 <- melt(d2, value.name="Bhat")
    d2[is.nan(d2[,3]),3] <- 0
    err <- inner_join(d1, d2)



    e <- err %>% mutate(error = Bobs-Bhat) %>% group_by(Var1) %>% 
        summarize(rmse=sqrt(mean(error^2)), mae=mean(abs(error)),
                  med=median(abs(error)))  %>%
        arrange(-rmse)  %>%
        mutate(nrmse=rmse/mean(rmse)) %>%
        left_join(idpop, by=c("Var1"="id"))
    e$is_outlier <- F

    pw <- err %>% filter(Var1 > Var2) %>% 
        left_join(idpop, by=c("Var1"="id")) %>%
        left_join(idpop, by=c("Var2"="id"))
    pw$is_outlier <- F
    pw <- pw %>% left_join(d3)

    return(list(error=e, pw=pw ))
}
