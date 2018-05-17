
plot_pw <- function(x, y, indiv_meta, pop_display, order, pdfname){
    o <- read.table(order)[,1:2]
    names(o) <- c('sampleId', 'n')
    o[,2] <- 1:nrow(o)

    r <- read.csv(indiv_meta)               
    s <- read.csv(pop_display)      
    rs <- merge(s,r, all.y=T)                             
    save.image()
    print(head(rs))
    rso <- merge(rs,o)
    rso <- rso[order(rso$n),]


    pdf(pdfname, width=11,height=11)
    plot(x, y, col=NULL)                                                      
    text(x, y, label=rso$name,
         col=as.numeric(rso$source)) 
    legend('topleft', legend=levels(r$source), col=1:100, lty=1)     

    dev.off()
}
