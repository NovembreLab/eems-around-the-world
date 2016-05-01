library(SpaceMix)

spacemix_wrapper <- function(cts, ss, pop_geo, output_name, model="target", ...){
run.spacemix.analysis(n.fast.reps = 10,
                        fast.MCMC.ngen = 1e5,
                        fast.model.option = model,
                        long.model.option = model,
                        data.type = "counts",
                        counts = cts,
                        sample.sizes = ss,
                        spatial.prior.X.coordinates = pop_geo$longitude,
                        spatial.prior.Y.coordinates = pop_geo$latitude,
                        round.earth = TRUE,
                        k = nrow(cts),
                        loci = ncol(cts),
                        ngen = 1e6,
                        printfreq = 1e2,
                        samplefreq = 1e3,
                        mixing.diagn.freq = 50,
                        savefreq = 1e4,
                        directory=output_name,
                        prefix = model)
}

plot_object <- function(opt, pop_geo, pop_display, ...){
    make.spacemix.map.list(
        MCMC.output.file=sprintf("%s/__LongRun/__space_MCMC_output1.Robj", opt),
        geographic.locations = pop_geo[,c('longitude', 'latitude')],
        name.vector = pop_display$name,
        color.vector = pop_display$color,
        quantile = 0.95,
        burnin = 0)
}

    args <- commandArgs(T)
    if(exists('snakemake')){
        ss = snakemake@input$ss
        cts = snakemake@input$cts
        pop_geo = snakemake@input$pop_geo
        model = snakemake@wildcards$model
        opt = snakemake@output[[1]]
    } else if(length(args) >=4){
        ss = args[1]
        cts = args[2]
        pop_geo = args[3]
        model = args[4]
        opt = args[5]
    }
    if(exists('ss')){
        samp_size <- t(read.csv(ss, check.names=F))
        counts <- t(read.csv(cts, check.names=F))
        pop_g <- read.csv(pop_geo)
        spacemix_wrapper(counts, samp_size, pop_g, 
            dirname(dirname(opt)))
    }
