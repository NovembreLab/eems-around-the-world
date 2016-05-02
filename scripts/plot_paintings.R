indiv_meta <- read.csv('subset/europe.indiv_meta', strings=F)                 
pop_display <- read.csv("../popres.pop_display", strings=F)                   
fam <- read.table("subset/europe.fam", strings=F)                             
fam <- data.frame(sampleId=fam[,1], n=1:nrow(fam))                 
q <- merge(merge(fam, indiv_meta), pop_display, all.x=T)           

q <- q[order(q$n),]

