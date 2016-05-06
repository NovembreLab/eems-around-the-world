source("scripts/eems_plot/full_plot.r")

args <- commandArgs(T)
nruns <- as.integer(args[1])
name <- args[2]
grid <- args[3]


mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)
datapath <- sprintf('eems/%s-run0', name)
plotpath <- sprintf('eemsout/%s_nruns%d', name, nruns)

plot.all.posterior.stuff(mcmcpath=mcmcpath,
                         datapath=datapath,
                         plotpath=plotpath,
                         grid=grid)
