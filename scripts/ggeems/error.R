suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(fields)
    library(scales)
})



get_marginal <- function(dist, pop_display){
    d2 <- dist %>% mutate(tmp = popId.y, popId.y = popId.x, popId.x=tmp) %>% 
        select(-tmp) %>% bind_rows(dist) %>%
        mutate(pcfit=pcDist10*0.0023+0.7073) %>%
        mutate(err=abs(eemsdist-gendist)/(gendist+0.001)) %>%
            group_by(popId.x) %>% summarize(err=median(err)) %>%
            arrange(-err) %>% rename(popId=popId.x) %>%
            left_join(pop_display) %>%
            ungroup %>% arrange(-err)
}

get_marginal_grid <- function(dist, pop_grid, pop_display){
    d2 <- dist %>% mutate(tmp = grid.y, grid.y = grid.x, grid.x=tmp) %>% 
        select(-tmp) %>% bind_rows(dist) %>%
        mutate(err=abs(eemsdist-gendist)/(gendist+0.001)) %>%
               group_by(grid.x) %>% summarize(err=median(err)) %>%
               arrange(-err) %>% rename(grid=grid.x) %>%
               inner_join(pop_grid) %>%      
               left_join(pop_display) %>%                            
               group_by(grid) %>%                           
               arrange(-n) %>%                              
               summarize(labels=paste(abbrev, collapse="+"),
                         err=mean(err)) %>%
               arrange(-err)
}

plot_error <- function(error, labels='abbrev', nmax=50){
    error$labels <- error[[labels]]
    error$labels <- factor(error$labels, levels=error$labels)
    error <- error[1:nmax,]
    if(!'is_outlier' %in% error) error$is_outlier <- F

    cm <- scale_fill_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))

    P <- ggplot(error) + geom_bar(aes(y=err, x=labels, 
                                      fill=is_outlier), stat='identity')  + cm
    P <- P + theme_classic()
    ymax <- pmin(1, max(error$err))
    P <- P + xlab("") + ylab("Rel. MAD")
#        P <- P + ggtitle("Error by Population")
    P <- P + theme(legend.position=0) 
    P <- P + scale_y_continuous(labels=function(i)sprintf("%.2f", i),
                                limits=c(0,ymax), oob=rescale_none)
#    P <- P + scale_y_continuous(labels=comma_format(digits=2))
    P <- P + cm +geom_hline(yintercept=0.1, color='lightgray')
    P <- P + cm +geom_hline(yintercept=0.05, color='lightgray')
    P <- P + theme(axis.text.x = element_text(size=rel(.6), angle=90))
}


plot_pw_pt <- function(df, var1, var2){
    ll <- lm(df[,var2] ~ df[,var1])
        r2 <- paste("r² = ",signif(summary(ll)$adj.r.squared, 2))
    cm <- scale_color_manual(labels=0:1, values=c('#aaaaaa', '#ffaaaa'))

    P <- ggplot(df) + geom_point(aes_string(y=var2, x=var1), 
                                size=1.5, alpha=.2, shape=16, color="#aaaaaa")
    P <- P + theme_classic() + 
        geom_abline(intercept=ll$coefficients[1], slope=ll$coefficients[2]) +
        theme(legend.position=0) +
        ylab("Genetic dissimilarity") +
        xlab("Fitted dissimilarity") + cm 
    P <- P+ 
        labs(title = r2) + 
        theme(plot.title = element_text(size = rel(.5), hjust=0)) +
        annotate("text", Inf, -Inf, label = r2, hjust = 1, vjust = -0.3, size=2)
    return(P)
}
