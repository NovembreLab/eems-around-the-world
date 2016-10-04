library(ggplot2)
library(dplyr)
library(fields)
library(reshape2)
source("scripts/eems_plot/load_output.r")

get_fit_matrix_full <- function(mcmcpath, indiv_label, pop_display){
    pop_display <- read.csv(pop_display)
    o <- read.table(sprintf("%s/ipmap.txt", mcmcpath[1]))
    names(o) <- 'grid'
    o <- cbind(grid=o, grid_order=1:nrow(o))
    indiv_label <- read.csv(indiv_label)     
    i2 <- bind_cols(indiv_label,grid=o) %>% left_join(pop_display)
    x <- i2 %>% group_by(grid) %>% 
        summarize(grid_order=first(grid_order), f=first(popId), a=first(name)) %>% 
        arrange(grid_order) %>% select(f, a) %>% 
        mutate(f=paste(as.character(a), as.character(f), sep="_"))
    return(x$f)
}
get_fit_matrix_abbrev <- function(mcmcpath, indiv_label, pop_display){
    pop_display <- read.csv(pop_display)
    o <- read.table(sprintf("%s/ipmap.txt", mcmcpath[1]))
    names(o) <- 'grid'
    o <- cbind(grid=o, grid_order=1:nrow(o))
    indiv_label <- read.csv(indiv_label)     
    i2 <- bind_cols(indiv_label,grid=o) %>% left_join(pop_display)
    x <- i2 %>% group_by(grid) %>% 
        summarize(grid_order=first(grid_order), f=first(abbrev), a=first(name)) %>% 
        arrange(grid_order) %>% select(f, a)
    return(x$f)
}
get_fit_matrix_ids <- function(mcmcpath, indiv_label, pop_display){
    pop_display <- read.csv(pop_display)
    o <- read.table(sprintf("%s/ipmap.txt", mcmcpath[1]))
    names(o) <- 'grid'
    o <- cbind(grid=o, grid_order=1:nrow(o))
    indiv_label <- read.csv(indiv_label)     
    i2 <- bind_cols(indiv_label,grid=o) %>% left_join(pop_display)
    x <- i2 %>% group_by(grid) %>% 
        summarize(grid_order=first(grid_order), f=first(popId), a=first(name)) %>% 
        arrange(grid_order) %>% select(f, a)
    return(x$f)
}

plot_within <- function(within){
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    P <- ggplot(within) + geom_text(aes(x=Wobs, y=What, label=label,
                                        color=is_outlier)) +
        cm  + theme_classic() + 
        geom_abline(intercept=0) + 
        theme(legend.position=0) +
        xlab("Observed within-population dissimilarity") +
        ylab("Fitted within-population dissimilarity") 
}

plot_pw <- function(df, outlier_id){
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    P <- ggplot(df) + geom_point(aes(x=Bobs, y=Bhat, 
                                  color=Var1 %in% outlier_id))  +
        theme_classic() + 
        geom_abline(intercept=0) +
        theme(legend.position=0) +
        xlab("Genetic dissimilarity") +
        ylab("Fitted dissimilarity") + cm
}
plot_vs_true <- function(df, outlier_id){
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    ll <- lm(Bobs ~ dist, data=df)
    P <- ggplot(df) + geom_point(aes(x=dist, y=Bobs, 
                                  color=Var1 %in% outlier_id))  +
        theme_classic() + 
        geom_abline(intercept=ll$coefficients[1], slope=ll$coefficients[2]) +
        theme(legend.position=0) + cm + 
        xlab("Geographic distance (km)") +
        ylab("Genetic dissimilarity")
}
plot_median_error <- function(pop_error){
    cm <- scale_fill_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))
    P <- ggplot(pop_error) + geom_bar(aes(y=pop_error, x=label, 
                                          fill=is_outlier), stat='identity') 
    P <- P + theme_classic()
    P <- P + xlab("") + ylab("Normalized Median Absolute Error")
#        P <- P + ggtitle("Error by Population")
    P <- P + theme(axis.text.x = element_text(size = rel(1), angle = 90))
    P <- P + theme(legend.position=0)
    P <- P + cm
}

ggscatter <- function(mcmcpath, pop_display_file, indiv_label_file, outnames,
                      outliers=NULL){
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

    pop_labels <- get_fit_matrix_abbrev(mcmcpath, indiv_label_file, pop_display_file)
    pop_ids <- get_fit_matrix_ids(mcmcpath, indiv_label_file, pop_display_file)
    pop_labels_full<- get_fit_matrix_full(mcmcpath, indiv_label_file, pop_display_file)
    label_mat <- outer(FUN=paste, pop_labels, pop_labels, sep="-")

    g <- read.output.graph(mcmcpath[1])

    Wobs <- diag(JtDobsJ) 
    What <- diag(JtDhatJ) 
    ones <- matrix(1,n_pops,1)                               
    Bobs <- JtDobsJ - (Wobs%*%t(ones) + ones%*%t(Wobs))/2   
    Bhat <- JtDhatJ - (What%*%t(ones) + ones%*%t(What))/2   
    dists <- rdist.earth(coords)
    dists <- melt(dists, value.name='dist')
    colnames(Bobs) <- NULL
    colnames(Bhat) <- NULL

    Bobs <- melt(Bobs, value.name='Bobs')
    Bhat <- melt(Bhat, value.name='Bhat')
    labels <- melt(label_mat, value.name='label')


    if(is.null(exfam)){
        outlier_pop <- c()
    }else{
        indiv_label <- read.csv(indiv_label_file)
		exfam <- read.table(exfam)
		excluded <- indiv_label$sampleId %>% setdiff(exfam[,1])
        outlier_pop <- indiv_label %>% filter(sampleId %in% excluded) %>% 
            select(popId) %>% unique()
    }
    outlier_id <- which(pop_ids %in% outlier_pop)

    df <- inner_join(Bobs, Bhat) %>% inner_join(labels)  %>% inner_join(dists)
    df$error <- abs(df$Bobs - df$Bhat)

    

    pop_error <- df %>% group_by(Var1) %>% summarize(pop_error=median(error))
    pop_error <- pop_error %>% cbind(label=pop_labels) %>% 
        arrange(-pop_error) %>%
        mutate(pop_error=pop_error/median(pop_error))

    pop_error$label <- factor(pop_error$label, levels=pop_error$label)                       
    pop_error <- pop_error %>% mutate(is_outlier=Var1 %in% outlier_id)
    
    within <- data.frame(label=as.character(pop_labels), Wobs=Wobs, What=What,
                         popId=pop_ids, is_outlier=pop_ids %in% outlier_pop)
    
    df <- df %>% filter(Var1 < Var2)

    ggsave(outnames[1], 
           plot_pw(df, outlier_id))
    ggsave(outnames[2], 
           plot_vs_true(df, outlier_id))
    ggsave(outnames[3], 
           plot_within(within))
    ggsave(outnames[4], 
           plot_median_error(pop_error))

}
