require(dplyr)

load_pop_meta <- function(pop_geo_file, pop_display_file){
	pg <- read.csv(pop_geo_file)
	pd <- read.csv(pop_display_file)
	pg %>% inner_join(pd)
}
