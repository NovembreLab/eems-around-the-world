library(dplyr)
library(reshape2)
require(fields)
require(readr)
require(abind)



D <- snakemake@input

outname <- snakemake@output[[1]]


#load mats
l <- lapply(D, read.csv)    

write.csv(v, outname, row.names=F)




