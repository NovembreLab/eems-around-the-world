# simple plot of smaples
do_plot <- function(name){
library(maps)
p <- read.table(sprintf("subset/%s.polygon", name))
a <- read.csv(sprintf("subset/%s.pop_geo", name))                     


pdf(sprintf("subset/%s_sample_map.pdf", name), width=8)
plot(p, type='l', col='red', lty=2, lwd=2); map(add=T)                           
text(a$longitude, a$latitude, a$popId, col='black', pch=16, cex=2) 
dev.off()
}



if(length(commandArgs) >=1)
	do_plot(commandArgs(T)[1])

