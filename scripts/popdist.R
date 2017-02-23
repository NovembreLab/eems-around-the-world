suppressPackageStartupMessages({
	library(dplyr)
})
#a <- read.table("data/world.diffs")
#b <- read.table("data/world.order", as.is=T)
#lab <- read.csv("/data/data_eems_other/meta/EuropeAllData.indiv_label", as.is=T)

b <- read.table(snakemake@input$order, as.is=T)
desc <- read.csv(snakemake@input$pop_display, as.is=T)
desc$popId <- as.character(desc$popId)
lab <- read.csv(snakemake@input$indiv_label, as.is=T)
a <- read.table(snakemake@input$diffs)

lab <- data.frame(sampleId=b[,1]) %>% left_join(lab)

tbl <- table(lab$popId)
pops <- as.character(names(tbl))
n <- length(pops)


J <- sapply(1:n, function(i)(pops[i] == lab$popId) / tbl[i])
D <- t(J) %*% as.matrix(a) %*% (J)
B <- diag(D)
D0 <- t(D - diag(D) /2) - diag(D)/2

abbrev <- data.frame(popId=pops) %>% left_join(desc)

rownames(D0) <- abbrev$name

colnames(D0) <- c(n, rep("", n-1))
write.table(D0, snakemake@output$dist, quote=F)
