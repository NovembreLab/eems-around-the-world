library(dplyr)
require(readr)
require(abind)



D <- snakemake@input

outname <- snakemake@output[[1]]


#load mats
l <- lapply(D, read.csv)    
merged <- l[[1]]
for(f in l[-1])
    merged <- inner_join(merged, f)

write.csv(merged, outname, row.names=F)




