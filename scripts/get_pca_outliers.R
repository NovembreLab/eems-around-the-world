
get_outlier_snp <- function(loadings="pca/flash_europe_dim20.load", 
                            bimfile="subset/europe.bim",
                            neighbors=5, region_bp=NULL, cutoff=0.005,
                            abs_cutoff=NULL,
                            return_snp_id=T){
    l <- read.table(loadings)
    bim <- read.table(bimfile, as.is=T)
    outlier_score <- apply(abs(l)/apply(l, 2, sd), 1, max) 
    if(is.null(abs_cutoff)){
        outlier_seeds <- which(outlier_score > quantile(outlier_score, 1-cutoff))
    } else {
        outlier_seeds <- which(outlier_score > abs_cutoff)
    }
    if(is.null(region_bp)){
        b <- neighbors

        outliers <- unique(c(sapply(outlier_seeds, function(i){
                                        pmax(1,i+(-b:b))
            })))
    } else{
        outlier_pos <- bim[outlier_seeds, c(1,4)]
        outliers <- unique(c(is_in_window(outlier_pos, bim, region_bp)))
    }
    if(return_snp_id)
        return(bim[outliers,2])
    return(outliers)
}

is_in_window <- function(pos, bim, region_bp=100000){
    n <- nrow(pos)
    m <- nrow(bim)
    x <- lapply(1:n, function(i){
            which(pos[i,1] == bim[,1] & 
                  pos[i, 2] + region_bp > bim[, 4] & 
                  pos[i, 2] - region_bp < bim[, 4])
    })
    unique(do.call(c, x))
}


loadings <- snakemake@input$loadings
bimfile <- snakemake@input$bim
region_bp <- snakemake@params$region_bp
abs_cutoff <- snakemake@params$abs_cutoff
g <- get_outlier_snp(loadings,
		     bimfile,
		     region_bp=region_bp,
		     abs_cutoff=abs_cutoff)
write.table(g, snakemake@output$outliers, row.names=F, col.names=F, quote=F)
save.image("QQQ.RData")

