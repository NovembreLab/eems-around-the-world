library(SpaceMix)
library(dplyr)

make.spacemix.map <- function (spacemix.map.list, text = FALSE, ellipses = TRUE, source.option = TRUE, 
    xlim = NULL, ylim = NULL, ...) {
    with(spacemix.map.list, {
        plot(MAPP.geogen.coords, type = "n", xlim = xlim, ylim = ylim, 
            xlab = "", ylab = "", ...)
        if (ellipses) {
            lapply(1:k, FUN = function(i) {
                plot.credible.ellipse(pp.geogen.ellipses[[i]], 
                  color.vector[i])
            })
        }
        if (text) {
            text(MAPP.geogen.coords, col = color.vector, font = 2, 
                labels = name.vector, cex = 0.7)
        }
        if (source.option) {
            if (ellipses) {
                lapply(1:k, FUN = function(i) {
                  plot.credible.ellipse(pp.admix.source.ellipses[[i]], 
                    admix.source.color.vector[i], fading = 1, 
                    lty = 2)
                })
            }
            text(MAPP.admix.source.coords, col = admix.source.color.vector, 
                font = 3, labels = name.vector, cex = 0.7)
            plot.admix.arrows(MAPP.admix.source.coords, MAPP.geogen.coords, 
                admix.proportions = MCMC.output$admix.proportions[, 
                  best.iter], colors = admix.source.color.vector, 
                length = 0.1)
        }
        box(lwd = 2)
    })
    return(invisible("spacemix map!"))
}


plot_object <- function(opt, pop_meta, ...){
    make.spacemix.map.list(
        #MCMC.output.file=sprintf("%s/__LongRun/__space_MCMC_output1.Robj", opt),
        MCMC.output.file=opt,
        geographic.locations = as.matrix(pop_meta[,c('longitude', 'latitude')]),
        name.vector = pop_meta$name,
        color.vector = pop_meta$color,
        quantile = 0.95,
        burnin = 0)
}

    args <- commandArgs(T)
    if(exists('snakemake')){
        spm_out = snakemake@input$spacemix_output
        pop_geo = snakemake@input$pop_geo
        pop_display = snakemake@input$pop_display
        opt = snakemake@output[[1]]
    } else if(length(args) >=4){
        spm_out = args[1]
        pop_geo = args[2]
        pop_display = args[3]
        opt = args[4]
    }
    if(exists('spm_out')){
        pop_g <- read.csv(pop_geo)
        pop_d <- read.csv(pop_display, strings=F)
        pop_meta <- pop_g %>% left_join(pop_d) %>% arrange(popId)
        #save.image('qqqtmpx')
        print("SDFAS")
	q <- SpaceMix::load_MCMC_output(spm_out)
	saveRDS(q, 'temp_q.rds')
        pobj <- plot_object(spm_out, pop_meta)
	saveRDS(pobj, 'temp.rds')
        png(opt, width=1600)
        make.spacemix.map(pobj, text=T, source.option=T)
#            xlim=range(pop_g$longitude),
#            ylim=range(pop_g$latitude))
#        require(maps)

        #map(add=T)
        dev.off()
    }
