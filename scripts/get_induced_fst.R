mfiles <- snakemake@input$mcmc

read_mcmcmhyper <- function(fname){
	m <- read.table(fname)[,1]
}

m <- mean(c(sapply(mfiles, read_mcmcmhyper)))

S <- function(k){

	g <- function(x)sqrt(3-4*cos(2*pi*x)+cos(2*pi*x)^2)
	f <- function(x,k=1)8 * (1-cos(2 * pi * x*k))/g(x)
i <- integrate(f, 0, 1, k=k)
return(i$value)
}

fst_logm <- function(m, k=1){
 1/(1+32 * 10^m/S(k)*4)
}
fstm <- function(m, S=1){
 1/(1+32 * m/S)
}

#res <- c(fst_logm(mean(m)), fstm(mean(10^m)))
res <- c(fst_logm(m), fst_logm(m, 2), fst_logm(m, 4))

write.table(res, snakemake@output$fst)

