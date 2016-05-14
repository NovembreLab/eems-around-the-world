library(SpaceMix)
library(dplyr)

spacemix_wrapper <- function(cts, ss, pop_geo, output_name, model, ...){
pop_geo <- pop_geo %>% arrange(popId)
run.spacemix.analysis(
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

			###production option set
                        ngen = 5e7,
                        samplefreq = 5e4,
                        fast.MCMC.ngen = 1e6,
                        savefreq = 1e6,
			n.fast.reps = 10,

			###debug option set
                        #ngen = 1e5,
                        #samplefreq = 1e3,
                        #fast.MCMC.ngen = 1e3,
                        #savefreq = 1e5,
			#n.fast.reps = 3,

                        mixing.diagn.freq = 50,
                        printfreq = 1e7,
                        directory=output_name,
                        prefix = "_")
}

    args <- commandArgs(T)
    if(exists('snakemake')){
        ss = snakemake@input$ss
        cts = snakemake@input$cts
        pop_geo = snakemake@input$pop_geo
        model = snakemake@wildcards$model
        opt = dirname(dirname(snakemake@output[[1]]))

        samp_size <- t(read.csv(ss, check.names=F))
        counts <- t(read.csv(cts, check.names=F))
        pop_g <- read.csv(pop_geo)
	print(opt)
        spacemix_wrapper(counts, samp_size, pop_g, opt, model)
    } else if(length(args) >=4){
        ss = args[1]
        cts = args[2]
        pop_geo = args[3]
        model = args[4]
	opt = dirname(dirname(args[5]))

        samp_size <- t(read.csv(ss, check.names=F))
        counts <- t(read.csv(cts, check.names=F))
        pop_g <- read.csv(pop_geo)
	print(opt)
        spacemix_wrapper(counts, samp_size, pop_g, opt, model)
    }
