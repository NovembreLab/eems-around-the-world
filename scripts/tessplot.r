
########################################
## R script for processing tess outputs
########################################
##
##Required packages: "fields" and "maps" 
##
########################################



# color palette easy to change
# (20) refers to the number of colors in the palette. 

colrp <- colorRampPalette(c("blue","white", "orange"))(20)

#extract spatial coordinates from the tess input file


tessplot = function( 
tessfile = "mydata.coord", 
qmatrix = "outfile.txt",
clustermap = 1,
colpalette = colrp,
mapadd = T
){


# read spatial coordinates from the tess input file  
coordinates = read.table(tessfile)[,1:2]

# read admixture (tess or clummp output)
cluster = read.table(qmatrix)[, clustermap + 1]

library(fields)

#If not installed, use the "Packages" menu and run "install packages", select "fields" in the proposed
#listing of packages


library(maps)

#If not installed, use the "Packages" menu and run "install packages", select "maps" in the proposed
#listing of packages

fit = Krig(coordinates, cluster, m= 1, theta = 10)
surface(fit, col = colpalette, levels = c(-1), extrap = T)
points(coordinates, cex = 1, pch = 16)
if(mapadd) map(add=T, interior = F, lwd = 1)

}





