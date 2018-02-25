require(reshape2)
source("scripts/language_analysis.R")

data <- list(
list("c1eumedi1nf",c("Slavic", "West Slavic", "East Slavic", "Balto-Slavic"),
  c("Germanic", "Western Germanic", "North Germanic"), c("Slavic", "Germanic")),
list("c1eumedi1nf",c("Slavic", "West Slavic", "East Slavic", "Balto-Slavic"),
     c("Caucasus", "Armenic", "Circassian", "Kartvelian", "Nakh-Daghestanian"), 
     c("Slavic", "Caucausus")),
list("c1eumedi1nf",c("Uralic"),c("Slavic", "West Slavic", "East Slavic",
                                 "Balto-Slavic")),
list("r1xasia1nf",c("Austroasiatic"), c("Dravidian")),
list("r1xasia1nf",c("NW-Indo-Aryan"), c("Central Indo-Aryan")),
list("r1xasia1nf",c("Indo-Aryan", "Eastern Indo-Aryan", "NW-Indo-Aryan", "Central Indo-Aryan"),
     		c("Dravidian")),
list("r1xasia1nf",c("Indo-Aryan", "Eastern Indo-Aryan", "NW-Indo-Aryan", "Central Indo-Aryan"),
     		c("Eastern Indian", "Dravidian", "Austroasiatic")),
list("c1africa2nf",c("Niger-Congo", "Atlantic-Congo", "Bantu", "Dogon"),
                     "Afro-Asiatic"),
list("c1africa2nf",c("Nilo-Saharan", "Mande", "Central Sudanic", "Nilotic"), 
  c("Niger-Congo", "Atlantic-Congo", "Bantu", "Dogon")),
list("c1africa2nf",
     c("KhoeSan", "Khoe", "Kxa", "Tuu"),
  c("Niger-Congo", "Atlantic-Congo", "Bantu", "Dogon")),
list("r1sa0",
     c("KhoeSan", "Khoe", "Kxa", "Tuu"),
  c("Niger-Congo", "Atlantic-Congo", "Bantu", "Dogon")),
list("c1africa2nf",
     c("KhoeSan", "Khoe", "Kxa", "Tuu"),
  c("Bantu")),
list("r1sa0",
     c("KhoeSan", "Khoe", "Kxa", "Tuu"),
  c("Bantu"))
)

plots <- lapply(data, function(row){
    language_plot(row[[1]], row[[2]], row[[3]]) +theme_classic(7)
})

plot_names <- sprintf("figures/dists/lang%s.png", 1:length(data))
mapply(ggsave,plot_names, plots, device="png", width=8, height=4, units="cm")

#mm <- lapply(data, function(l)do_mantel(l[[1]], l[[2]], l[[3]]))
#m2 <- lapply(data, function(l)do_mantel2(l[[1]], l[[2]], l[[3]]))

m3 <- do_mantel3("r1xasia1nf", 
    c("Indo-Aryan", "Eastern Indo-Aryan", "NW-Indo-Aryan", "Central Indo-Aryan"),
    c("Dravidian"),
    c("Austroas.", "Austroasiatic"))
P3 <- language_plot3("r1xasia1nf", 
    c("Indo-Aryan", "Eastern Indo-Aryan", "NW-Indo-Aryan", "Central Indo-Aryan"),
    c("Dravidian"),
    c("Austroas.", "Austroasiatic"))+ theme_classic(7)
ggsave("figures/dists/3lang.png", device="png", width=8, height=4, units="cm")
l <-  list("c1africa2nf",c("Niger-Congo", "Atlantic-Congo", "Bantu", "Dogon"),
                     "Afro-Asiatic",
                    c("Nilo-Saharan", "Mande", "Central Sudanic"))
m3b <- do_mantel3(l[[1]], l[[2]], l[[3]], l[[4]])
P3b <- language_plot3(l[[1]], l[[2]], l[[3]], l[[4]]) + theme_classic(7)
ggsave("figures/dists/3langb.png", device="png", width=8, height=4, units="cm")
    
