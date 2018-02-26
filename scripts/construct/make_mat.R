
traw <- read.table(snakemake@input$traw, header=T)
fam <- read.table(snakemake@input$fam)

mat <- t(as.matrix(traw[,-c(1:6)]))


rownames(mat) <- fam[,1]
colnames(mat) <- traw$SNP

if(snakemake@params$max_snp < ncol(mat)){
    max_snp <- snakemake@params$max_snp
	mat <- mat[,1:max_snp]
}

print(dim(mat))
print(dim(fam))
print(snakemake@params$max_snp)


 saveRDS(mat, snakemake@output$rds)
