plot_freq_vs_HWE<-function(file,title="",                                        
    cols=c("#ffb000", "#89bff7", "#004400"), ...){                                    
                                                                                 
    plink_hwe <- read.table(file, as.is=TRUE, header=T, ...)                          
                                                                                 
    counts <- sapply(plink_hwe$GENO,function(x){as.numeric(strsplit(x,"/")[[1]])})
        counts<-t(counts)                                                        
        tot_counts<-rowSums(counts)                                              
        geno_freq<-counts / tot_counts                                           
        allele_freq<-(geno_freq[,1]+.5 *geno_freq[,2])                           
                                                                                 
        ss_allele<-c(allele_freq,1-allele_freq)                                  
        ss_geno<-rbind(geno_freq,geno_freq[,c(3,2,1)])                           
                                                                                 
    col_alpha <- adjustcolor(cols, alpha.f=0.05)
    allele_jitter <- ss_allele
    #allele_jitter <- jitter(ss_allele, 1.5)
    #ss_geno <- jitter(ss_geno, 1.5)
    #ss_geno <- pmax(pmin(ss_geno, 1), 0)
    #allele_jitter <- pmax(pmin(allele_jitter, 1), 0)
    plot(allele_jitter,ss_geno[,1],xlim=c(0,1),ylim=c(0,1.5),col=col_alpha[1],         
        xlab="allele frequency",ylab="genotype frequency",main=title, pch=16)            
    points(allele_jitter,ss_geno[,3],xlim=c(0,1),ylim=c(0,1),col=col_alpha[2], pch=16)       
    points(allele_jitter,ss_geno[,2],xlim=c(0,1),ylim=c(0,1),col=col_alpha[3], pch=16)       
                                                                                 
    smooth=1/5                                                                   
    bad_gt <- is.na(rowSums(ss_geno))
    ss_geno <- ss_geno[!bad_gt,]
    ss_allele <- ss_allele[!bad_gt]
    lines(lowess(ss_geno[,1]~ss_allele,f = smooth),col="black", lwd=4)           
    lines(lowess(ss_geno[,3]~ss_allele,f = smooth),col="black", lwd=4)           
    lines(lowess(ss_geno[,2]~ss_allele,f = smooth),col="black", lwd=4)           
                                                                                 
        x=1:1000/1000                                                            
        lines(x,x^2,lty=2, lwd=4)                                                       
        lines(x,2*x*(1-x),lty=2, lwd=4)                                                 
        lines(x,(1-x)^2,lty=2, lwd=4)                                                   
        legend(x=0.0,y=1.5,col=c(cols,rep("black",2)),                             
            legend=c("Homozygote AA","Homozygote aa","Heterozygote Aa",          
                "Mean","Hardy Weinberg Expectation"),pch=c(rep(16,3),            
                    rep(NA,2)),lty=c(rep(NA,3),1,2))                             
}
png(snakemake@output$fig, width=900, height=900)
plot_freq_vs_HWE(snakemake@input$hwe)
dev.off()
