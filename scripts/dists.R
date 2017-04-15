source("scripts/config.R")
source("scripts/load_pop_meta.R")
require(viridis)
require(scales)
library(ggplot2)

breaks_factory <- function(n_ticks, min_lower=NULL, max_upper=NULL){
    return(function(x){
    l = round(ceiling(x[1]*100)/100, 2)
    #upper bound is at most 1
    u = round(floor(x[2]*100)/100, 2)
    r <- u-l
    ll <- max(min_lower, l + 0.10 * r)
    uu <- min(max_upper, u - 0.10 * r)
    s <- signif(seq(ll ,uu, length.out=n_ticks), 2)
    print(s); 
    return(s)
})}




C <- get_config(snakemake, plotname='scatter')

dists <- load_dists(snakemake@input$dists,
                    snakemake@input$pop_display)
dists <- dists %>% mutate(label=sprintf("%s|%s", abbrev.x, abbrev.y)) %>%
    select(one_of(c(C$xvars, C$yvar, "label"))) 

reg <- lapply(C$xvars, function(v){
          form <- as.formula(sprintf(" %s ~ %s ", C$yvar, v))
          l <- lm(form, data=dists)
          res <- c(l$coefficients, summary(l)$r.squared)
          abs_res <- abs(l$residuals)
          return(list(vars=res, res=abs_res))
        })
saveRDS(reg, "tmp.rds")

#regression summary statistics
reg.var <- sapply(reg, function(i)i$var)
rownames(reg.var) = c("intercept", "y", "rsq")
colnames(reg.var) = C$xvars

#get residuals from regression
residuals_df <- as.data.frame(lapply(reg, function(i)i$res))
names(residuals_df) <- C$xvars
residuals_df %>% melt(value.name="residual") -> residuals_melt


dists %>%  
    melt(id.vars=c(C$yvar, "label")) %>%
    bind_cols(residuals_melt %>% select(residual)) ->d2
levels(d2$variable) <- c("geographic", 
                         "EEMS-fitted",
                         "PCA-fitted")
reg.df <- data.frame(t(reg.var), variable=levels(d2$variable))




outlier_cutoff <- d2 %>% group_by(variable) %>% 
    summarize(Q = quantile(residual, 1-C$outlier_pct))
#outlier_cutoff <- d2 %>% group_by(variable) %>% 
#    summarize(Q = 0.05)

outliers <- d2 %>% left_join(outlier_cutoff) %>% 
    filter(residual>Q) %>% select(-Q) 
inliers <- d2 %>% left_join(outlier_cutoff) %>% 
    filter(residual<=Q) %>% select(-Q) 

g <- ggplot(inliers, aes(x=value, y=gendist, label=label))

if(C$point_type == 'text'){
    g <- g + geom_text(color=C$color, size=C$size,  alpha=C$alpha)
} else if (C$point_type == 'hex'){
    n_bins <- max(C$height, C$width) * 10
    g <- g + geom_hex( color=NA, size=C$size,bins=n_bins,  alpha=C$alpha)  +
        scale_fill_continuous(low = "gray85", high = "black", trans = sqrt_trans())

} else {
    g <- g + geom_point(color=C$color, size=C$size,  alpha=C$alpha)
}

if(C$outlier_type == 'text'){
    g <- g + geom_text(data=outliers, aes(x=value, y=gendist, label=label),
                       color='#ffaaaa', size=C$outlier_size,  alpha=C$outlier_alpha) 
} else {
    g <- g + geom_point(data=outliers, aes(x=value, y=gendist, label=label),
                        color='#ffaaaa', size=C$outlier_size,  alpha=C$outlier_alpha) +
        scale_size_identity()
}

if(C$tall){
g <- g + theme_classic() +
    facet_wrap(~variable, scales="free_y", ncol=1
               ) + coord_flip()
} else{
g <- g + theme_classic() +
    facet_wrap(~variable, scales="free_x", ncol=3)
}

#add regression stuff
g <- g + geom_abline(data=reg.df, aes(intercept=intercept, slope=y),
                     color="red", size=.2, linetype=2) 

reg.df <- reg.df %>% mutate(rsq= paste("r² = ",signif(rsq, 2)))
r2 <- "r²"
#g <- g + annotate("text", Inf, -Inf, label = r2, hjust = 1, vjust = -0.3, size=2)


if(C$tall){
g <- g + geom_text( data=reg.df, aes(label=rsq), y=Inf, x=-Inf, 
                    hjust=1, vjust=-0.3, size=C$rsq_size)
} else {
g <- g + geom_text( data=reg.df, aes(label=rsq), x=Inf, y=-Inf, 
                    hjust=1, vjust=-0.3, size=C$rsq_size)
}

#    facet_grid(.~variable, scales="free_x")
g <- g + theme_classic(base_size=8) + 
    theme(axis.title=element_blank(), 
          strip.background=element_blank(),
         axis.text = element_text(size=rel(.8))) + 
        theme(legend.position="none") +
        scale_y_continuous(breaks=breaks_factory(3)) +
        scale_x_continuous(breaks=breaks_factory(3, 0, 10000))


ggsave(snakemake@output$png, g, width=C$width, height=C$height)
saveRDS(g, snakemake@output$rds)

save.image("debug.rdata")

