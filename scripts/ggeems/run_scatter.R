source("scripts/load_pop_meta.R") #load raw
source("scripts/ggeems/scatter.R")
args <- commandArgs(T)
nruns <- as.integer(args[1])
name <- args[2]

mcmcpath <- sprintf('eemsout/%d/%s/', 0:(nruns-1), name)

pop_display <- args[3]
pop_geo_file <- args[4]
indiv_label <- args[5]

diffs <- args[6]
order <- args[7]

exfam <- args[8]
exname <- args[9]

if(is.null(exfam)){
    outnames <- sprintf("eemsout_gg/%s_nruns%s-scatter0%d.png", name, nruns, 1:7)
} else {
    outnames <- sprintf("eemsout_gg/%s_nruns%s_ex:%s-scatter0%d.png", name, nruns, exname, 1:7)
}

print(outnames)

ggscatter(mcmcpath, diffs, order, pop_display, pop_geo_file, indiv_label, outnames, exfam)
