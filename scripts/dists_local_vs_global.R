source("scripts/config.R")
source("scripts/ggeems/error.R")
source("scripts/load_pop_meta.R")
library(ggplot2)
library(reshape2)
library(dplyr)

panel_names <- c("Afro-Eurasia", 
            "W. Eurasia", 
            "C/E. Eurasia", 
            "SE-Asia",
            "Africa",
            "KhoeSan")
#            "SA Bantu")

panel_files <- c("c1global1nfd",
                 "c1eumedi1nf",
                 "r1xasia1nf",
                 "r1seasia0nf",
                 "c1africa2nf",
                 "r1sa1hg")
#                 "r1sabantu0")
simple_plot <- function(dist, v){
    par(mfrow=c(1,2))
    plot(dist$gendist, v[[1]]$res, pch='.') 
    plot(dist$gendist, v[[2]]$res, pch='.') 
}

ggplot1<- function(...){
    q <- ggplot(df) + 
        geom_hex(aes(x=gendist, y=value)) + 
        facet_grid(~variable)
}

cumerror_plot <- function(...){
    x <- df %>% dplyr::filter(variable=="pcDist10") %>% 
        arrange(gendist) %>% mutate(v=abs(value)) 
    y <- df %>% dplyr::filter(variable=="eemsdist") %>% 
        arrange(gendist) %>% mutate(v=abs(value)) 
    y_error <- cumsum(y$v)/1:nrow(y)
    x_error <- cumsum(x$v)/1:nrow(x)
    lim <- c(0, max(x_error, y_error))
                                                                             
    plot(y$gendist, y_error, type='l', ylim=lim)
    lines(x$gendist, x_error, col=2)                  
}

cum_error <- function(x) x %>% arrange(gendist) %>%
    mutate(error=abs(error)) %>%
    mutate(cum_error =cumsum(error)/1:nrow(x))

pct_error <- function(x, n_bins=10){
    if(n_bins==1){k <- x %>% mutate(gdistbin=factor(1))
    }else{
    k <- x %>% mutate(gdistbin = cut(gendist, n_bins, labels=F))
    }
k
}


pop_display <- "pgs/gvar3.pop_display"

make_cum_error_df <- function(){
    dist_files <- sprintf("dists/%s.dist", panel_files)
    f <- function(...)load_dists(...) %>% 
        select(gendist, eemsdist, pcDist10)
    dists <- lapply(dist_files, f, pop_display)
    names(dists)<- panel_names
    df <- bind_rows(dists, .id="panel")

    u <- df %>% group_by(panel) %>% 
        do(get_residuals(.)) %>%
        group_by(panel, var_error) %>% 
        do(cum_error(.))     
    u$panel <- factor(u$panel, levels=panel_names)
    u
}

make_pct_error_df <- function(n_bins=10){
    dist_files <- sprintf("dists/%s.dist", panel_files)
    f <- function(...)load_dists(...) %>% 
        select(gendist, eemsdist, pcDist10)
    dists <- lapply(dist_files, f, pop_display)
    names(dists)<- panel_names
    df <- bind_rows(dists, .id="panel")

    u <- df %>% group_by(panel) %>% 
        do(get_residuals(.)) %>%
        group_by(panel, var_error) %>% 
        do(pct_error(., n_bins=n_bins))     
    u$panel <- factor(u$panel, levels=panel_names)
    u$gdistbin <- as.factor(u$gdistbin)
    u %>% group_by(panel, var_error, gdistbin) %>%
        summarize(e=median(abs(error)))
}

pct_error_all_plot <- function(df){
    library(plyr)
    df$var_error <- revalue(df$var_error, c("pc_error"="PCA", "eems_error"="EEMS"))
    g <- ggplot(df) + 
        geom_col(aes(x=gdistbin, y=e, group=var_error,
                     fill=var_error), position=position_dodge()) +
        facet_wrap(~panel, scale="free_x", ncol=6) +
        theme_classic(base_size=7) +
        theme(legend.position="right", legend.title=element_blank()) +
        xlab("Genetic Distance Bin") +
        ylab("Residual Error")
        #ylab("Median Absolute Residual")
}


cum_error_all_plot <- function(df){
    g <- ggplot(df) + 
        geom_line(aes(x=gendist, y=cum_error, 
                      group=var_error, color=var_error)) +
        facet_wrap(~panel, scales="free_x", ncol=2) +
        theme_classic() + 
        theme(legend.position="left") + 
        xlab("Genetic Distance") +
        ylab("Cumulative Error")
}

dist_file <- "dists/xasia0f.dist"
dist_file <- "dists/global0fg.dist"
dist_file <- "dists/global0fg.dist"
dist_file <- "dists/eumedi1f.dist"

dist <- load_dists(dist_file, pop_display)
dist_err <- get_marginal(dist, read.csv(pop_display) )

#C <- get_config(snakemake, plotname="distfit")

C <- list()
C$xvars <- c("eemsdist", 'pcDist10')
C$yvar <- 'gendist'

get_regression <- function(C, dists){
    reg <- lapply(C$xvars, function(v){
          form <- as.formula(sprintf(" %s ~ %s ", C$yvar, v))
          l <- lm(form, data=dists)
          res <- c(l$coefficients, summary(l)$r.squared)
          abs_res <- abs(l$residuals)
          return(list(vars=res, res=l$residuals, abs_res=abs_res))
        })
}
get_residuals <- function(dists){
    reg <- get_regression(C, dists)
    data.frame(eems_error=reg[[1]]$res,
              pc_error=reg[[2]]$res,
              gendist=dists$gendist) %>% 
              melt(id=c("gendist"))  %>%
    dplyr::rename(var_error=variable, error=value)
    
}

df1 <- make_pct_error_df(n_bins=1)
write.table(df1, "dists/mad_all.txt")
df <- make_pct_error_df()
write.table(df %>% dplyr::filter(gdistbin==1), "dists/mad10.txt")
g <- pct_error_all_plot(df)

ggsave("figures/dists/fit_by_bin.png", width=7, height=1.5)
