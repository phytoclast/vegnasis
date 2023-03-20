#This function summarises cover by stratum membership and growth habit. Inputs require a data frame processed by clean.veg and  user supplied vector of stratum height breaks. ----

summary.strata <-  function(x, breaks=c(0.5,5,15)){
  y <- NULL
  nbks <- length(breaks)+1
  brks <- c(0,breaks,1000)
  for(i in 1:(nbks)){#i = 8
    y0 <- x %>% subset(ht.max < brks[i+1] & ht.max >= brks[i])

    if(nrow(y0)>0){
      y0 <- y0 %>% mutate(stratum=i, stratum.label = paste0(brks[i], "-", ifelse(i==nbks, "+",brks[i+1])), bottom= brks[i], top = ifelse(i==nbks,brks[i]+1,brks[i+1]))
      y1 <- y0 %>% group_by(plot, type, stratum, stratum.label, bottom, top) %>% summarise(Cover = cover.agg(cover))

    if(is.null(y)){y <- y1}else{y <- rbind(y, y1)}}
  }
  return(y)
}

#This function summarize crown overlap by stratum and growth habit. Inputs require a data frame processed by clean.veg and  user supplied vector of stratum height breaks. ----
breaks <- c(0.1, 0.5, 2, 5, 10, 20, 30)
summary.crown.thickness <-  function(x, breaks=c(0.5,5,15)){
  y <- NULL
  nbks <- length(breaks)+1
  brks <- c(0,breaks,1000)
  for(i in 1:(nbks)){#i = 5
    y0 <- x %>% subset(ht.min < brks[i+1] & ht.max >= brks[i])

    if(nrow(y0)>0){
      y0 <- y0 %>% mutate(stratum=i, stratum.label = paste0(brks[i], "-", ifelse(i==nbks, "+",brks[i+1])), bottom= brks[i], top = ifelse(i==nbks,brks[i]+1,brks[i+1]))
      y1 <- y0 %>% group_by(plot, type, stratum, stratum.label, bottom, top) %>% summarise(Cover = cover.agg(cover))

    if(is.null(y)){y <- y1}else{y <- rbind(y, y1)}}
  }
  return(y)
}

#This function adds zeros to plots lacking cover values found in other plots ----

structure.fill.zero <- function(x){
  x.plot <- unique(subset(x, select=c("plot")))
  x.mid <- unique(subset(x, select=c("type","stratum","stratum.label","bottom","top")))
  x.fill <- merge(x.plot, x.mid) |> mutate(Cover = 0)
  x.fill <- rbind(x, x.fill)
  x.fill <- x.fill |> group_by(plot,type,stratum,stratum.label,bottom,top) |> summarise(Cover = max(Cover))
  return(x.fill)}
