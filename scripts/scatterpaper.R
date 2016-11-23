library(fields)


to.dis <- function(mat){
n <- dim(mat)[1]
m1 <- matrix(diag(mat), n, n)
m1 + t(m1) - 2 * mat
}

NPC <- 2
folder <- 'medi3'

if(length(commandArgs(T)) > 0)folder <- commandArgs(T)[1]

runs <- 0:3
hatnames <- sprintf('eemsout/%s/%s/rdistJtDhatJ.txt', runs, folder)
obsname <- sprintf('eemsout/%s/%s/rdistJtDobsJ.txt', runs[1], folder)
coords <- sprintf('eemsout/%s/%s/demes.txt', runs[1], folder)

hats <- lapply(hatnames, function(h) as.matrix(read.table(h)))
hat <- Reduce(`+`, hats)/length(runs)
obs <- read.table(obsname)
obs[is.na(obs)] <- mean(diag(as.matrix(obs)), na.rm=T)
#obs <- obs[-55,-55]

n <- nrow(obs)
J <- diag(n) - 1/n
mat <- - J %*% as.matrix(obs/2) %*% J
e <- eigen(mat)
if(NPC==1){
pca_approx <- e$vectors[,1:1, drop=F] %*% e$values[1] %*% t(e$vectors[,1, drop=F])
} else{
pca_approx <- e$vectors[,1:NPC] %*% diag(e$values[1:NPC]) %*% t(e$vectors[,1:NPC])
}



hmat <- - J %*% as.matrix(hat/2) %*% J

dmat <- rdist.earth(read.table(coords)[1:n,])

mat.d <- to.dis(mat)
hmat.d <- to.dis(hmat)
pca_approx.d <- to.dis(pca_approx)

diag(hmat.d) <- NA
diag(mat.d) <- NA
diag(pca_approx.d) <- NA
diag(dmat) <- NA

regs <- list(lm(c(dmat) ~ c(mat.d)),
lm(c(hmat.d) ~ c(mat.d)),
lm(c(pca_approx.d) ~ c(mat.d))
)

rsq <- c(
summary(lm(c(dmat) ~  c(mat.d)))$adj.r.squared,
summary(lm((c(hmat.d)) ~  (c(mat.d))))$adj.r.squared,
summary(lm(c(pca_approx.d) ~  c(mat.d)))$adj.r.squared
)

plot_stuff <- function(...){
par(mfrow=c(3,1), mar=c(4,4,1,1))
plot(dmat~ mat.d, xlab="Genetic distance", ylab="Geographic distance",...)
abline(regs[[1]])
plot(hmat.d~ mat.d, xlab="Genetic distance", ylab="EEMS-fitted distance",...)
abline(regs[[2]])
plot(pca_approx.d~ mat.d, xlab="Genetic distance", ylab="PCA-fitted distance",...)
abline(regs[[3]])
}

fname=sprintf('eemsout/scatterpaper_%s.png', folder)
dpi=300
png(file=fname, width=3*dpi, height=6*dpi, res=dpi)
plot_stuff(col='#88888855', pch=16)
dev.off()

write.table(file=sprintf('eemsout/scatterpaper_%s.rsq.txt', folder[1]), rsq,
row.names=F, col.names=F, quote=F)




