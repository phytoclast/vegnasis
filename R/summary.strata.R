#This function summarises cover by stratum membership and growth habit. Inputs require a data frame processed by clean.veg and  user supplied vector of stratum height breaks. ----

summary.strata <-  function(x, breaks){
  y <- NULL
  nbks <- length(breaks)+1
  brks <- c(0,breaks,1000)
  for(i in 1:(nbks)){#i = 8
    y0 <- x %>% subset(ht.max < brks[i+1] & ht.max >= brks[i])

    if(nrow(y0)>0){
      y0 <- y0 %>% mutate(stratum=i, stratum.label = paste0(brks[i], "-", ifelse(i==nbks, "+",brks[i+1])))
      y1 <- y0 %>% group_by(vegplotid, planttypegroup, stratum, stratum.label) %>% summarise(Cover = cover.agg(cover))

    if(is.null(y)){y <- y1}else{y <- rbind(y, y1)}}
  }
  return(y)
}

#This function summarises crown overlap by stratum and growth habit. Inputs require a data frame processed by clean.veg and  user supplied vector of stratum height breaks. ----
breaks <- c(0.1, 0.5, 2, 5, 10, 20, 30)
summary.crown.thickness <-  function(x, breaks){
  y <- NULL
  nbks <- length(breaks)+1
  brks <- c(0,breaks,1000)
  for(i in 1:(nbks)){#i = 5
    y0 <- x %>% subset(ht.min < brks[i+1] & ht.max >= brks[i])

    if(nrow(y0)>0){
      y0 <- y0 %>% mutate(stratum=i, stratum.label = paste0(brks[i], "-", ifelse(i==nbks, "+",brks[i+1])))
      y1 <- y0 %>% group_by(vegplotid, planttypegroup, stratum, stratum.label) %>% summarise(Cover = cover.agg(cover))

    if(is.null(y)){y <- y1}else{y <- rbind(y, y1)}}
  }
  return(y)
}
