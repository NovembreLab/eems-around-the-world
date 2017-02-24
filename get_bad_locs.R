require(dplyr)
sample_plot <- function(s="easia0"){

a <- read.table(sprintf("subset/%s.polygon",s )) 
b <- read.csv(sprintf("subset/%s.pop_geo", s)) 
require(maps)

plot(a, type='l', col=2); map(add=T)

points(b[,3:2], col=3)
}

get_uncertain_dudes <- function(){
s="easia0"
b <- read.csv(sprintf("subset/%s.pop_geo", s)) 
locs <- read.csv("locations_all.csv")
m <- merge(locs,b, by.x='ID', by.y='popId') 

locs %>% filter(Certainty.group!=1) %>% 
    select(ID) %>% unlist() %>% 
    cat(file="easia_certainty_1.txt", sep=",")                         

locs %>% filter(Certainty.group %in% c(2,3)) %>% 
    select(ID) %>% unlist() %>% 
    cat(file="easia_certainty_2.txt", sep=",")                         

locs %>% filter(Certainty.group %in% c(2)) %>% 
    select(ID) %>% unlist() %>% 
    cat(file="easia_certainty_3.txt", sep=",")                         

}
