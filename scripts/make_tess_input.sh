args <- commandArgs(T)
input_fam <- args[1]
input_indiv_meta <- args[2]
input_pop_geo <- args[3]
output_tess <- args[4]

fam <- read.table(input_fam)                                                       
fam <- data.frame(sampleId=fam[,1], n=1:nrow(fam))                                     
indiv_meta <- read.csv(input_indiv_meta)                                           
pop_geo <- read.csv(input_pop_geo)                                                 
indiv_meta <- merge(indiv_meta, pop_geo)                                               
m <- merge(fam, indiv_meta)                                                            
m <- m[order(m$n),]                                                                    
print(names(m))                                                                        
write.table(m[,c('longitude', 'latitude')], output_tess, row.names=F, col.names=F) 

