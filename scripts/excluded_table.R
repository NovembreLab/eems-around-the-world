library(dplyr)
ex <- read.csv("subset/excluded.txt") 
v <- read.csv("/data/meta/pgs/gvar.pop_display")
ex %>% left_join(v)

