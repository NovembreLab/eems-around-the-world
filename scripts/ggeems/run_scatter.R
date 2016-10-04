source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/scatter.R")
args <- commandArgs(T)
nruns <- as.integer(args[1])
name <- args[2]

mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)

pop_display <- args[3]
pop_geo <- args[4]
indiv_label <- args[5]

exfam <- args[6]
exname <- args[7]

if(is.null(exfam)){
    outnames <- sprintf("eemsout_gg/%s_nruns%s-scatter0%d.png", name, nruns, 1:4)
} else {
    outnames <- sprintf("eemsout_gg/%s_nruns%s_ex:%s-scatter0%d.png", name, nruns, exname, 1:4)
}

print(outnames)

ggscatter(mcmcpath, pop_display, indiv_label, outnames, exfam)
