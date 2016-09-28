library(ggmap)
library(rgdal)
library(jpeg)
source("eems.plots.ggplot.R")
FILEFOLDER="./"
setwd(FILEFOLDER)


# debug eems
mcmcpath <- sprintf('/data/eems_pipeline/eemsout/%s/africa/', 0:3)
add_eems_overlay <- function(P, mcmcpath, longlat=T, alphaplot=T, ...){

    infiles <- c("ipmap.txt", "demes.txt", "edges.txt")
    mcmcpath <- check.files.at.path(infiles, mcmcpath)

    n_runs <- length(mcmcpath)

    if(n_runs == 0) return( 0 )
    message("processing the following EEMS output")
    message(paste0(mcmcpath,collate="\n"))
    
    dimns <- read.dimns(mcmcpath[1], longlat=longlat)


    average.eems.contours.ggplot(P, mcmcpath, dimns, longlat, plot.params=list(),
                                 is.mrates=T, alphaplot=alphaplot, ...)
    #ggsave(paste0(plotpath,"-mrates.pdf"))
    #average.eems.contours.ggplot(P, mcmcpath, dimns, longlat, plot.params=list(),
    #                             is.mrates=F, alphaplot=alphaplot, ...)
    #ggsave(paste0(plotpath,"-qrates.pdf"))
    #dev.off()
}




#GET maps and coordinates
map <- get_map(maptype="terrain",location=c(7.734375,38.8225909761771,9.84375,41.50857729743936), zoom=8);
a=ggmap(map)
#FORMAT axis
a=a+scale_x_continuous("Longitude",limits = c(8, 10.3), expand = c(0, 0))
a=a+ scale_y_continuous("Latitude",limits = c(38.85, 41.38), expand = c(0, 0))
a=a+theme(axis.text.x=element_text(size=25),axis.title.x=element_text(size=25))
a=a+theme(axis.text.y=element_text(size=25),axis.title.y=element_text(size=25))
a
#GET tile coordinate
tiles=read.table(paste(FILEFOLDER,"Tile_coordinates.txt",sep="/"),header=T)

#PLOT tiles as raster 
for (i in 1:dim(tiles)[1]) {
  img=readJPEG(paste(FILEFOLDER,"/9_",tiles[i,1],".jpg",sep=""))            
  r=as.raster(img)
  a=a+inset_raster(r,xmin=tiles[i,2],xmax=tiles[i,3],ymin=tiles[i,4],ymax=tiles[i,5])
}
#READ town position and names
towns=read.table(paste(FILEFOLDER,"Town_coordinates.txt",sep="/"),header=T)
towns

a = add_eems_overlay(a, mcmcpath)

#PLOT town points and segments
a=a+geom_point(data=towns[,2:3],size=4)	
a=a+geom_segment(data=towns,aes(xend=xend,yend=yend),size=2) 

#a=a+geom_rect(xmin=rep(9.9,4),xmax=rep(10.3,4),ymin=towns$yend-0.05,ymax=towns$yend+0.05,col=towns$colour,fill=rep("white",4))
a=a+geom_rect(data=towns,xmin=towns$xend,xmax=10.3,ymin=towns$yend-0.05,ymax=towns$yend+0.05,col="black", fill=as.character(towns$colour),size=2) #colour=as.character(towns$colour)
a=a+geom_text(data=towns,aes(x=xend+0.02,y=yend,label=Town,hjust=0),size=10,colour="white")


#READ provinces and plot circles
provinces=read.table(paste(FILEFOLDER,"Province_coordinates.txt",sep="/"),header=T)
provinces
a=a+geom_point(data=provinces,size=40,colour="black")	
a=a+geom_point(data=provinces,size=38,alpha=1,colour=provinces$colour)	
a=a+geom_text(data=provinces,label=provinces$province,size=11)

#READ shapefile with provinces boundaries

provinces=readOGR(dsn=FILEFOLDER,layer="limitiAmministrProvinciali")
provinces <- spTransform(provinces, CRS("+proj=longlat +datum=WGS84"))
provinces=fortify(provinces)
#PLOT province polygons
a=a+geom_polygon(aes(x=long, y=lat, group=group), size=1,color='black', data=provinces,alpha=0)

#disable eems legends
a = a+theme(legend.position="none")

png("SardiniaEEMSMap.png",width=1600,height = 1600)
plot(a)
dev.off()

