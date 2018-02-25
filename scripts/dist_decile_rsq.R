source("scripts/config.R")
source("scripts/load_pop_meta.R")
require(viridis)
require(scales)
require(dplyr)
library(ggplot2)
xvars <- c("geoDist", "eemsdist", "pcDist2", "pcDist10")
yvar <- "gendist"


dists <- load_dists(snakemake@input$dists,
                    snakemake@input$pop_display)

#dists <- dists %>% filter(gendist < quantile(eemsdist, .10))

reg <- lapply(xvars, function(v){
          form <- as.formula(sprintf(" %s ~ %s ", yvar, v))
          l <- lm(form, data=dists)
          res <- c(l$coefficients, summary(l)$r.squared)
          abs_res <- abs(l$residuals)
          return(list(vars=res, res=abs_res))
        })

#regression summary statistics
reg.var <- sapply(reg, function(i)i$var)
rownames(reg.var) = c("intercept", "y", "rsq")
colnames(reg.var) = xvars

write.csv(reg.var[3,], snakemake@output$rsq)

save.image("QQQ")
