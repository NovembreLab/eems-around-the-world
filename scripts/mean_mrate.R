require(yaml)
require(dplyr)

e10 <- function(x)10^x

data_sets <- c("global0fg", "eumedi1f", "xasia1fP", "seasia3fcP", 
               "africa0fbP", "r1sa0", "r1sabantu0", "seasia0ff",
               "easiacer20b","easiacer20", "europe3", "europe2", "europe0f",
               "r1sahg0nf")
res <- c(480, 120, 240, 120, 240, 120, 120, 240, 240, 120, 240, 120, 120, 120)/120


path <- sprintf("eemsout/0/%s/mcmcmhyper.txt", data_sets)

x <- sapply(path, function(x)read.table(x) %>% select(V1) %>% unlist %>% e10 %>%
            mean )
fst <- 1/ (1 + 32 * x * sqrt(res))
fst2 <- 1/ (1 + 32 * x  )
