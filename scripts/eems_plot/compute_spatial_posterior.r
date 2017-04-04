require(FNN)

#' compute spatial posterior from eems output
#'
#' using the data from spatfile (read with read.spatial.files)
#' find for each point in dimns$mark the mean and variance of migration rate


compute.pts <- function(dimns,spatfile, standardize=F) {
    ## Here 'seeds' stores the generator seeds of a Voronoi tessellation
    ## and 'rates' stores the log10-transformed rates of the tiles.
    ## If there are C seeds in the partition, then 'seeds' is a matrix
    ## with C rows and 2 columns and 'rates' is a vector with C elements
    n_samples <- length(spatfile$rates)
    n_points <- sum(dimns$filter)
    zvals <- matrix(0, nrow=n_samples, ncol=n_points) 
    for(i in 1:n_samples){
        query.pts <- dimns$marks[dimns$filter,]
        ref.pts <- cbind(spatfile$xseed[[i]], spatfile$yseed[[i]])
        n <- knnx.index(ref.pts, query.pts, k=1)
        query.rate <- spatfile$rates[[i]][n]
        #zvals <- rbind(zvals, query.rate)
        zvals[i,] <- query.rate
    }

    if(standardize){
	zvals <- exp(log(zvals) - rowMeans(log(zvals)))
    }

    return(zvals)
}

