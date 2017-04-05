require(dplyr)
require(data.table)

load_pop_meta <- function(pop_geo_file, pop_display_file){
	pg <- read.csv(pop_geo_file)
	pd <- read.csv(pop_display_file)
	pg %>% inner_join(pd)
}

load_pca_data <- function(pc, fam,
                      indiv_meta, pop_display){
    indiv_meta <- read.csv(indiv_meta)
    pop_display <- read.csv(pop_display)
    indiv <- merge(indiv_meta, pop_display, all.x=T)

    fam <- read.table(fam)[,1]

    data <- data.frame(fread(pc))
    names(data) <- paste0("PC", 1:ncol(data))

    data <- cbind(fam, data, n=1:length(fam)) 
    names(data)[1] <- 'sampleId'
    m <- merge(indiv, data, all.y=T)
    m <- m[order(m$n),]

    return(m)
}

load_pca_median <- function(pc, pop_display){
    pop_display <- read.csv(pop_display)
    data <- read.csv(pc)

    data %>% left_join(pop_display) %>% arrange (-PC1_M)
}

get_geo_dmat <- function(coords){
    dmat <- rdist.earth(coords, miles=F) 
    geo_dist_list <- melt(dmat, value.name="dist")

}

get_pop_mats <- function(mcmcpath, diffs, order, pop_display_file, 
			 indiv_label_file, pop_geo_file){
    d <- as.matrix(read.table(diffs))
    ord <- as.matrix(read.table(order))
    odf <- data.frame(sampleId=ord[,1], v=1:nrow(ord))

    pop_geo <- read.csv(pop_geo_file)
    geo_dist_list <- get_geo_dmat(coords[,c('longitude', 'latitude')])

    ipmap <- sprintf("%s/ipmap.txt", mcmcpath[1])
    i2 <- get_grid_info(ipmap, indiv_label_file, pop_display_file)
    idpop <- data.frame(popId=pop_geo$popId, id=1:nrow(pop_geo))
     v <- idpop$id          
     names(v) <- idpop$popId



     uid <- i2 %>% dplyr::select(popId, grid) %>% unique 
     uid$Var1 <- v[as.character(uid$popId)]      



    n <- nrow(i2)
    n_grid <- max(i2$grid)
    n_pops <- length(unique(i2$popId))
    J <- matrix(0, nrow=n, ncol=n_grid)       
    J[cbind(1:n, i2$grid)] <- 1 #/i2$n
    ssJ <- colSums(J)
    ssJmat <- ssJ %*% t(ssJ)
    diag(ssJmat) <- diag(ssJmat) - ssJ

    K <- matrix(0, nrow=n, ncol=n_pops)       
    K[cbind(1:n, v[as.character(i2$popId)])] <- 1#/i2$popn
    ssK <- colSums(K)
    ssK <- pmax(ssK, 1)
    ssKmat <- ssK %*% t(ssK)
    diag(ssKmat) <- diag(ssKmat) - ssK
    ssKmat <- pmax(ssKmat, 1)

    #d1 is the matrix of avg pw differences of dudes between pops
    d1 <- (t(K) %*% d %*% K)/ssKmat
    d1[is.nan(d1)] <- 0

    JtDhatJ <- matrix(0,n_grid,n_grid)
    for (path in mcmcpath) {
        JtDhatJ <- JtDhatJ + 
            as.matrix(read.table(sprintf('%s/rdistJtDhatJ.txt',path)),header=F)
    }
    n_reps <- length(mcmcpath)
    JtDhatJ <- JtDhatJ/n_reps

    p2g <- uid$grid; names(p2g) <- uid$Var1                               
    d2 <- matrix(0, nrow=n_pops, ncol=n_pops)                             
    for(i in 1:n_pops)for(j in 1:n_pops)
        d2[i,j] <- JtDhatJ[ p2g[i], p2g[j] ]   



    pop_labels <- get_labels_abbrev(i2)
    pop_ids <- get_labels_ids(i2)
    pop_labels_full<- get_labels_full(i2)
    label_mat <- outer(FUN=paste, pop_ids, pop_ids, sep=":")
    labels <- melt(label_mat, value.name='label')
    grid_ids <- data.frame(Var1=1:length(pop_ids),
                      'labels'=pop_ids)


    l <- subtract_diag(d1, d2)
    Bobs <- l$Bobs
    Bhat <- l$Bhat
    Wobs <- l$Wobs
    What <- l$What
    
    df <- inner_join(Bobs, Bhat) %>% inner_join(geo_dist_list) %>% 
        inner_join(uid) 
    df$error <- abs(df$Bobs - df$Bhat)

    pop_error <- df %>% group_by(Var1) %>% 
        summarize(E=median(Bhat), M=mean(Bhat)) %>% 
        left_join(df) %>%
        group_by(Var1, popId) %>%
        #summarize(mad=median(error), rmse=sqrt(mean(error^2)),
        #          M=first(M), E=first(E)) %>%
        #mutate(nrmse=rmse/M, nmad=mad/E) %>%
        summarize(nmad=median(error/(Bobs+0.001)), 
                  nmad_=sqrt(mean((error^2/(Bobs+0.001))))) %>%
        left_join(uid) %>% 
        arrange(-nmad)
        #select(-M) %>%
        #arrange(-nmad) %>%
        #mutate(is_outlier=Var1 %in% outlier_pop$grid)

    pop_error$is_outlier <- F

    pop_error_pw <- df %>% select(-popId) %>% filter(Var1 > Var2) %>% 
        left_join(idpop, by=c("Var1"="id")) %>%
        left_join(idpop, by=c("Var2"="id"))
    pop_error_pw$is_outlier <- F
    pop_error_pw <- pop_error_pw %>% left_join(geo_dist_list)

    #df for within grid error
    within <- data.frame(uid, Wobs=Wobs, What=What)

    return(list(error=pop_error, pw=pop_error_pw, within=within ))
}

get_grid_info <- function(ipmap_file, indiv_label_file){
    pop_display <- read.csv(pop_display_file)
    o <- read.table(ipmap_file)
    names(o) <- 'grid'
    o <- cbind(grid=o, grid_order=1:nrow(o))
    indiv_label <- read.csv(indiv_label_file)     
    i2 <- bind_cols(indiv_label,grid=o) %>% left_join(pop_display)
}

load_dists <- function(dist_file, pop_display){
    dists <- read.csv(dist_file)
    pd <- read.csv(pop_display)
    dists %>% left_join(pd, by=c(popId.x="popId")) %>% 
	left_join(pd, by=c(popId.y="popId"))
}
