source("scripts/config.R")
source("scripts/load_pop_meta.R")
library(ggplot2)
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
reg.df <- data.frame(t(reg.var), variable=colnames(reg.var))

#get residuals from regression
residuals_df <- as.data.frame(lapply(reg, function(i)i$res))
names(residuals_df) <- C$xvars
residuals_df %>% melt(value.name="residual") -> residuals_melt


dists %>%  
    melt(id.vars=c(C$yvar, "label")) %>%
    bind_cols(residuals_melt %>% select(residual)) ->d2



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
} else {
    g <- g + geom_point(color=C$color, size=C$size,  alpha=C$alpha)
}

if(C$outlier_type == 'text'){
    g <- g + geom_text(data=outliers, aes(x=value, y=gendist, label=label),
                       color='#ffaaaa', size=C$outlier_size,  alpha=C$outlier_alpha)
} else {
    g <- g + geom_point(color=C$color, size=C$outlier_size,  alpha=C$alpha)
}

g <- g + theme_classic() +
    facet_grid(.~variable, scales="free_x")

#add regression stuff
g <- g + geom_abline(data=reg.df, aes(intercept=intercept, slope=y),
                     color="red", size=.2, linetype=2) 

reg.df <- reg.df %>% mutate(rsq= paste("r² = ",signif(rsq, 2)))
r2 <- "r²"
#g <- g + annotate("text", Inf, -Inf, label = r2, hjust = 1, vjust = -0.3, size=2)
g <- g + geom_text( data=reg.df, aes(label=rsq), x=Inf, y=-Inf, 
                   hjust=1, vjust=-0.3, size=2)

#    facet_grid(.~variable, scales="free_x")

ggsave(snakemake@output$png, g, width=C$width, height=C$height)
saveRDS(g, snakemake@output$rds)

