require(dplyr)
require(vegan)
require(ggplot2)
require(readr)

read_lang <-function()read_csv("pgs/gvar3.language2", comment="#")  %>% rename(language=linguistic_group)
language_plot <- function(name, LANG1, LANG2){
	print(name)
	print("------------")
	print(LANG1)
	print("------------")
	print(LANG2)
    
    f1 <- sprintf("subset/%s.indiv_meta", name)
    f2 <- sprintf("dists/%s.dist", name)

    lang <- read_lang()
    im <- read_csv(f1)
    dists <- read_csv(f2)


    d <- dists %>% left_join(lang, by=c(popId.x="popId")) %>% 
        rename(lang.x=language) %>%
        left_join(lang, by=c(popId.y="popId")) %>% 
        rename(lang.y=language)

    d$lang.x[is.na(d$lang.x)] <- "other"
    d$lang.y[is.na(d$lang.y)] <- "other"

    #d$color = "grey"
    #d$color[d$lang.x == d$lang.y& d$lang.x != "other"] <- 'black'
    #d$color[d$lang.x != d$lang.y& d$lang.x != "other"& d$lang.y != "other"] <- 'red'
    
    d$color = "grey"
    d$color <- factor(d$color, levels=c( sprintf("within %s", LANG1[1]),
                                         sprintf("within %s", LANG2[1]),"grey", "between"))
    diff_lang <- (d$lang.x %in% LANG1 & d$lang.y %in% LANG2) | (d$lang.x %in% LANG2 & d$lang.y %in% LANG1)
    same1_lang <- (d$lang.x %in% LANG1 & d$lang.y %in% LANG1) 
    same2_lang <- (d$lang.x %in% LANG2 & d$lang.y %in% LANG2) 
    d$color[diff_lang] <- "between"
    d$color[same1_lang] <- sprintf("within %s", LANG1[1])
    d$color[same2_lang] <- sprintf("within %s", LANG2[1])

    print("------------")
    print(names(d))
    print("------------")

    d$label <- sprintf("%s|%s", d$abbrev.x, d$abbrev.y)
    
    d <- d[d$color != "grey",]

    d <- d %>% mutate(geoDist = geoDist / 1000)

    P <- ggplot(d) + geom_text(aes(x=geoDist, y=gendist,label=label, color=color), size=1.5) +
        facet_wrap(~color) +
        xlab("geographic distance (10³ km)") +
        ylab("genetic distance") +
        scale_size_identity() +
        guides(color=FALSE)

    m <- do_mantel(name, LANG1, LANG2)
    s = sprintf("M = %s; p = %s", signif(m$statistic, 3), signif(m$signif, 3))
    mantel.df <- data.frame(color="between", text=s)
    P <- P + geom_text(data=mantel.df, aes(label=text), x=Inf, y=-Inf, 
		       hjust=1, vjust=-0.3, size=1.5)

    return(P)
}

do_mantel <- function(name, LANG1, LANG2){
    print(name)

    f1 <- sprintf("subset/%s.indiv_meta", name)
    f2 <- sprintf("dists/%s.dist", name)

    lang <- read_lang()
    im <- read_csv(f1)
    dists <- read_csv(f2)


    d <- dists %>% left_join(lang, by=c(popId.x="popId")) %>% 
        rename(lang.x=language) %>%
        left_join(lang, by=c(popId.y="popId")) %>% 
        rename(lang.y=language)
        to_keep <- c(LANG1, LANG2)

        d2 <- d %>% dplyr::filter(lang.x %in% to_keep, lang.y %in% to_keep) %>%
            mutate(diff.lang=!((lang.x %in% LANG1 & lang.y %in% LANG1) |
                             (lang.x %in% LANG2 & lang.y %in% LANG2)) )

        d_flip <- mutate(d2, tmp=popId.x, popId.x=popId.y, popId.y=tmp) %>%
            select(-tmp)
        d2 <- bind_rows(d2, d_flip)
        ldist <- dcast(d2, popId.x ~ popId.y, value.var="diff.lang", fun=mean) %>% 
            select(-popId.x) %>% as.matrix
        gdist <- dcast(d2, popId.x ~ popId.y, value.var="gendist", fun=mean) %>% 
            select(-popId.x) %>% as.matrix
        geodist <- dcast(d2, popId.x ~ popId.y, value.var="geoDist", fun=mean) %>% 
            select(-popId.x) %>% as.matrix

        diag(ldist) <- 0
        diag(gdist) <- 0
        diag(geodist) <- 0

        mantel.partial(ldist, gdist, geodist)
}

do_mantel3 <- function(name, LANG1, LANG2, LANG3){

    f1 <- sprintf("subset/%s.indiv_meta", name)
    f2 <- sprintf("dists/%s.dist", name)

    lang <- read_lang()
    im <- read_csv(f1)
    dists <- read_csv(f2)


    d <- dists %>% left_join(lang, by=c(popId.x="popId")) %>% 
        rename(lang.x=language) %>%
        left_join(lang, by=c(popId.y="popId")) %>% 
        rename(lang.y=language)
        to_keep <- c(LANG1, LANG2, LANG3)

        d2 <- d %>% dplyr::filter(lang.x %in% to_keep, lang.y %in% to_keep) %>%
            mutate(diff.lang=!((lang.x %in% LANG1 & lang.y %in% LANG1) |
                             (lang.x %in% LANG2 & lang.y %in% LANG2) |
                             (lang.x %in% LANG3 & lang.y %in% LANG3)  ))

        d_flip <- mutate(d2, tmp=popId.x, popId.x=popId.y, popId.y=tmp) %>%
            select(-tmp)
        d2 <- bind_rows(d2, d_flip)
        ldist <- dcast(d2, popId.x ~ popId.y, value.var="diff.lang", fun=mean) %>% 
            select(-popId.x) %>% as.matrix
        gdist <- dcast(d2, popId.x ~ popId.y, value.var="gendist", fun=mean) %>% 
            select(-popId.x) %>% as.matrix
        geodist <- dcast(d2, popId.x ~ popId.y, value.var="geoDist", fun=mean) %>% 
            select(-popId.x) %>% as.matrix

        diag(ldist) <- 0
        diag(gdist) <- 0
        diag(geodist) <- 0

        mantel.partial(ldist, gdist, geodist)
}
do_mantel2 <- function(name, LANG1, LANG2){
    print(name)

f1 <- sprintf("subset/%s.indiv_meta", name)
f2 <- sprintf("dists/%s.dist", name)

lang <- read_lang()
im <- read_csv(f1)
dists <- read_csv(f2)


d <- dists %>% left_join(lang, by=c(popId.x="popId")) %>% 
    rename(lang.x=language) %>%
    left_join(lang, by=c(popId.y="popId")) %>% 
    rename(lang.y=language)
    to_keep <- c(LANG1, LANG2)

    d2 <- d %>% dplyr::filter(lang.x %in% to_keep, lang.y %in% to_keep) %>%
        mutate(diff.lang=!((lang.x %in% LANG1 & lang.y %in% LANG1) |
                         (lang.x %in% LANG2 & lang.y %in% LANG2)) )

    d_flip <- mutate(d2, tmp=popId.x, popId.x=popId.y, popId.y=tmp) %>%
        select(-tmp)
    d2 <- bind_rows(d2, d_flip)
    ldist <- dcast(d2, popId.x ~ popId.y, value.var="diff.lang", fun=mean) %>% 
        select(-popId.x) %>% as.matrix
    gdist <- dcast(d2, popId.x ~ popId.y, value.var="gendist", fun=mean) %>% 
        select(-popId.x) %>% as.matrix
    geodist <- dcast(d2, popId.x ~ popId.y, value.var="geoDist", fun=mean) %>% 
        select(-popId.x) %>% as.matrix

    diag(ldist) <- 0
    diag(gdist) <- 0
    diag(geodist) <- 0

    mantel(ldist, geodist)
}
language_plot3 <- function(name, LANG1, LANG2, LANG3){
	print(name)
	print("------------")
	print(LANG1)
	print("------------")
	print(LANG2)
	print("------------")
	print(LANG3)
    
    f1 <- sprintf("subset/%s.indiv_meta", name)
    f2 <- sprintf("dists/%s.dist", name)

    lang <- read_lang()
    im <- read_csv(f1)
    dists <- read_csv(f2)


    d <- dists %>% left_join(lang, by=c(popId.x="popId")) %>% 
        rename(lang.x=language) %>%
        left_join(lang, by=c(popId.y="popId")) %>% 
        rename(lang.y=language)

    d$lang.x[is.na(d$lang.x)] <- "other"
    d$lang.y[is.na(d$lang.y)] <- "other"

    #d$color = "grey"
    #d$color[d$lang.x == d$lang.y& d$lang.x != "other"] <- 'black'
    #d$color[d$lang.x != d$lang.y& d$lang.x != "other"& d$lang.y != "other"] <- 'red'
    
    d$color = "grey"
    d$color <- factor(d$color, levels=c( sprintf("within %s", LANG1[1]),
                                         sprintf("within %s", LANG2[1]),
                                         sprintf("within %s", LANG3[1]),"grey", "between"))
    L <- c(LANG1, LANG2, LANG3)
    diff_lang <- !((d$lang.x %in% LANG1 & d$lang.y %in% LANG1) |
                   (d$lang.x %in% LANG2 & d$lang.y %in% LANG2) |
                   (d$lang.x %in% LANG3 & d$lang.y %in% LANG3)  )
    diff_lang <- diff_lang & d$lang.x %in% L & d$lang.y %in% L
    same1_lang <- (d$lang.x %in% LANG1 & d$lang.y %in% LANG1) 
    same2_lang <- (d$lang.x %in% LANG2 & d$lang.y %in% LANG2) 
    same3_lang <- (d$lang.x %in% LANG3 & d$lang.y %in% LANG3) 
    d$color[diff_lang] <- "between"
    d$color[same1_lang] <- sprintf("within %s", LANG1[1])
    d$color[same2_lang] <- sprintf("within %s", LANG2[1])
    d$color[same3_lang] <- sprintf("within %s", LANG3[1])

    d$label <- sprintf("%s|%s", d$abbrev.x, d$abbrev.y)
    
    d <- d[d$color != "grey",]

    d <- d %>% mutate(geoDist = geoDist / 1000)

    P <- ggplot(d) + geom_text(aes(x=geoDist, y=gendist,label=label, color=color), size=1.5) +
        facet_grid(~color) +
        xlab("geographic distance (10³ km)") +
        ylab("genetic distance") +
        scale_size_identity() +
        guides(color=FALSE)

    m <- do_mantel3(name, LANG1, LANG2, LANG3)
    s = sprintf("M = %s; p = %s", signif(m$statistic, 3), signif(m$signif, 3))
    mantel.df <- data.frame(color="between", text=s)
    P <- P + geom_text(data=mantel.df, aes(label=text), x=Inf, y=-Inf, 
		       hjust=1, vjust=-0.3, size=1.5)


    return(P)
}
