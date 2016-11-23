source("scripts/eems_plot/full_plot.r")

outdir = 'eemsout'

args <- commandArgs(T)
nruns <- as.integer(args[1])
name <- args[2]
grid <- args[3]



pop_display <- args[4]
pop_geo <- args[5]
indiv_label <- args[6]


if(!is.na(args[7])){print(args[7]); if(args[7] == "0")outdir='eemsout0'}

mcmcpath <- sprintf('%s/%d/%s/', outdir, 0:(nruns-1), name)
datapath <- sprintf('eems/%s-run0', name)
plotpath <- sprintf('%s/%s_nruns%d', outdir, name, nruns)

plot.all.posterior.stuff(mcmcpath=mcmcpath,
                         datapath=datapath,
                         plotpath=plotpath,
                         grid=grid, 
			 pop_display=pop_display,
			 pop_geo=pop_geo,
                         indiv_label=indiv_label)
