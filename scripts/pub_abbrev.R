a <- read.csv("pgs/gvar3.indiv_meta")
pub_abbrev <- names(table(a$wasDerivedFrom)) 

pub_abbrev <- data.frame(a=pub_abbrev)
x <- 
    c("J17", "Be10", "B13", "Bi10", 
      "B09", "C14", "C11", "D13", 
      "E14", "F13", "H09", "K14",
      "L14", "M11", "M13", "Pa14",
      "Pi14", "N08", "H10",
      "R13", "Ra11", "R10", "S14",
      "Re11", "V14", "X10", "X11", "Y12", "Y15")

pub_abbrev$abbrev <- x

names(pub_abbrev) <- c("wasDerivedFrom","pub")

write.csv(pub_abbrev, "pgs/gvar3.pub") 
