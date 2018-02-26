#args <- commandArgs(T)
#input_fam <- args[1]
#input_indiv_meta <- args[2]
#input_pop_geo <- args[3]
#output_tess <- args[4]

input_fam <- snakemake@input$fam
input_indiv_meta <- snakemake@input$indiv_meta
input_pop_geo <- snakemake@input$pop_geo
output_tess <- snakemake@output$tess

fam <- read.table(input_fam)                                                       
fam <- data.frame(sampleId=fam[,1], n=1:nrow(fam))                                     
print(dim(fam))
indiv_meta <- read.csv(input_indiv_meta)                                           
print(dim(indiv_meta))
pop_geo <- read.csv(input_pop_geo)                                                 
print(dim(pop_geo))
indiv_meta <- merge(indiv_meta, pop_geo)                                               
print(dim(indiv_meta))
m <- merge(fam, indiv_meta)                                                            
print(dim(m))
m <- m[order(m$n),]                                                                    
print(names(m))                                                                        
print(dim(m))
write.table(m[,c('longitude', 'latitude')], output_tess, row.names=F, col.names=F) 

