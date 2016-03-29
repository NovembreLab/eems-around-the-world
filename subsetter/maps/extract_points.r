require(rgdal)
require(sp)
require(fields)


run <- function(full_grid="mytest",
                poly="/data/eems-project/SGDP/SGDP_v1/input/world.outer",
                samples="/data/eems-project/SGDP/SGDP_v1/input/world.coord",
                out="grid"){
    my.polygon <- read.table(poly)       
    print("done reading")
    tract <- load.shape(full_grid)
    print("done reading")
    coords <- extract.points(tract)
    print("done reading")
    edges <- extract.edges(tract, coords)
    print("done reading")
    coords <- wrap.america(coords)
    print("done reading")
    samples <- read.table(samples)
    print("done reading")

    edges.filtered <- filter.edges(coords, edges, my.polygon)
    print("done filtering")
    n.pops <- sum(as.logical(point.in.polygon(coords[,1],
                                               coords[,2],
                                               my.polygon[,1],   
                                               my.polygon[,2])))
    coords.filtered <- coords[as.logical(point.in.polygon(coords[,1],
                                               coords[,2],
                                               my.polygon[,1],   
                                               my.polygon[,2])),]

    qmap <- get.vertex.seq(coords, coords.filtered)
    edges.final <- cbind(qmap[edges.filtered[,1]], qmap[edges.filtered[,2]])
    edges.final <- matrix(unlist(edges.final), ncol=2)
    edges.final <- t(apply(edges.final, 1, sort))
    edges.final <- unique(edges.final, margin=1)
    pts <- get.sample.deme(samples, coords.filtered)
    distmat <- rdist.earth(samples, coords.filtered) 
    sample.assignment <- apply(distmat, 1, which.min)
    write.files(out, coords.filtered, edges.final, sample.assignment)
}

make.plot <- function(my.polygon, coords, edges, e2){
    require(maps)
    plot(my.polygon, type="l")
    for.plot(coords, edges)
    segments(x1, y1, x2, y2, col="grey")
    for.plot(coords, e2)
    segments(x1, y1, x2, y2, col="red")
    map(add=T, col="black")
}
make.plot2 <- function(my.polygon, coords, edges, samples, pts){
    require(maps)
    plot(my.polygon, type="l")
    for.plot(coords, edges)
    segments(x1, y1, x2, y2, col="grey")
    points(samples, pch=3, col="blue")
    points(pts, pch=16, col="red")
    map(add=T, col="black")
}

load.shape <- function(file){
    b = basename(file)
    d = dirname(file)
    tract <- readOGR(dsn = d, layer=b)
    return(tract)
}

get.vertex.seq <- function(k, coords){

    f <- apply(k, 1, function(x)which(abs(x[1] - coords[,1])< 1e-4 & 
                                      abs(x[2] - coords[,2])< 1e-4 ))
    #f <- sapply(f, function(x){if(length(x)==0) return(NA); return(x)})
    return(f)
}

extract.points <- function(polys){
    a <- slot(polys, "polygons")

    n.polys = length(a)

    coords <- c()
    for( i in 1:n.polys){
        cur = a[[i]]
        cur.poly <- slot(cur, "Polygons")[[1]]
        cur.coords <- slot(cur.poly, "coords")
        coords <- rbind(coords, cur.coords)
    }
    coords <- round(coords, digits=5)
    coords <- unique(coords)
    return(coords)
}

extract.edges <- function(polys, coords){
    a <- slot(polys, "polygons")
    n.polys = length(a)

    edges <- c()
    for( i in 1:n.polys){
        cur = a[[i]] 
        cur.poly <- slot(cur, "Polygons")[[1]]
        cur.coords <- slot(cur.poly, "coords")
        vs <- get.vertex.seq(cur.coords, coords)

        for(j in 1:(length(vs)-1)){
            tmp <- vs[j:(j+1)]
            edges<-rbind(edges, tmp) 
        }
    }

    return(edges)
    edges <- matrix(unlist(edges), ncol=2)
    edges <- t(apply(edges, 1, sort))
    edges <- unique(edges, margin=1)
    return(edges)

}

get.sample.deme <- function(samples, coords.filtered){
    distmat <- rdist.earth(samples, coords.filtered) 
    sample.assignment <- apply(distmat, 1, which.min)
    pts <- coords.filtered[apply(distmat, 1, which.min),]     
    return(pts)
}

for.plot <- function(coords, edges){
    x1 <<- coords[edges[,1],1]   
    x2 <<- coords[edges[,2],1]   
    y1 <<- coords[edges[,1],2]   
    y2 <<- coords[edges[,2],2]   



}

filter.edges <- function(coords, edges, my.polygon){
    kp <- as.logical(point.in.polygon(coords[,1], coords[,2], my.polygon[,1],
                                      my.polygon[,2])  )
    kp <- which(kp)
    to.keep <- apply(edges, 1, function(x)all(x %in% kp))  
    e2 <- edges[to.keep,]
    e2 <- e2[abs(coords[e2[,1],1] - coords[e2[,2],1]) < 300,]
    return(e2)
}

wrap.america <- function(coords){
    to.flip <- coords[,1] < (-30)
    coords[to.flip,1] <- coords[to.flip,1] + 360
    return(coords)
}


write.files <- function(grid="grid", coords.filtered, edges.final,
                        sample.assignment){
    fnames <- paste0(grid, c(".demes",".edges",".ipmap"))
    write.table(coords.filtered, fnames[1], row.names=F, col.names=F)
    write.table(edges.final, fnames[2], row.names=F, col.names=F)
    write.table(sample.assignment, fnames[3], row.names=F, col.names=F)

}


if(length(commandArgs(T)>0)){
    ca <- commandArgs(T)
    run(ca[1], ca[2], ca[3], ca[4])
}
