source("scripts/eems_plot/full_plot.r")

args <- commandArgs(T)
nruns <- as.integer(args[1])
name <- args[2]
grid <- args[3]


mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)
datapath <- sprintf('eems/%s-run0', name)
plotpath <- sprintf('eemsout/%s_nruns%d', name, nruns)

pop_display <- args[4]
pop_geo <- args[5]
indiv_label <- args[6]

plot.all.posterior.stuff(mcmcpath=mcmcpath,
                         datapath=datapath,
                         plotpath=plotpath,
                         grid=grid, 
			 pop_display=pop_display,
			 pop_geo=pop_geo,
                         indiv_label=indiv_label)
