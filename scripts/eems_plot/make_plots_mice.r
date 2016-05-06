source("src/full_plot.r")

args <- commandArgs(T)


if(length(args) > 0)
    cont <- args[1]
if(length(args) > 1)
    grid <- as.numeric(args[2])
if(length(args) > 2)
    v <- args[3]


mcmcpath <- sprintf('sample_data/%s/output/%s/650_run%d', v ,cont, 0:9)
datapath <- sprintf('sample_data/%s/input/%s', v, cont)
plotpath <- sprintf('sample_data/%s/output/%s', v, cont)

plot.all.posterior.stuff(mcmcpath=mcmcpath,
                         datapath=datapath,
                         plotpath=plotpath,
                         grid=grid)
