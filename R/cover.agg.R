#This function aggregates cover among taxa of a given stratum or guild, assuming random crown distributions, and where totals cannot not exceeding 100%.

#' Aggregate canopy cover
#'
#' This function aggregates cover among taxa of a given stratum or guild, assuming random crown distributions, and where totals cannot not exceeding 100 percent. This function is called within many vegetation summary functions. Essentially the same as a method suggested by an equation in Appendex K of the US National Vegetation Classification (USNVC, 2008). The crown avoidance option gives more priority for stems located in opening rather than in the shade of another crown.
#'
#' @param x Cover of individual taxa or strata to aggregate.
#' @param avoid TRUE: Crowns compete for light and avoid overlapping; FALSE: Crowns are completely random.
#'
#' @returns Aggregate cover
#' @export
#'
#' @examples x <- c(5,5,10,20)
#' @examples cover.agg(x, avoid=TRUE)
#' @examples cover.agg(x, avoid=FALSE)
cover.agg <- function(x, avoid=FALSE){
  if (max(x) > 100){warning("Cover cannot exceed 100%!")}else{
    if(!avoid){k = round(100*(1-10^(sum(log10(1-(x/100.0001))))),1)
    }else{
      k = round(tocov(sum(carea(x, avoid = TRUE)), avoid = TRUE),1)
    }
    if(max(x) == 100){k=100}
    return(k)}}

#get aggregate crown area
#' Aggregate canopy cover from summed crown area
#'
#'
#' @param kk Total crown area
#' @param avoid TRUE: Crowns compete for light and avoid overlapping; FALSE: Crowns are completely random.
#'
#' @returns Aggregate canopy cover
#' @export
#'
#' @examples
tocov <- function(kk, avoid=T){
  if(avoid){
    #avoid overlap
    b1=5.504e+01;b2=2.081e-02;b3=1.489e+02;b4=9.593e+03
    k = 2 * (100/(1 + exp(0 - kk/b1))^1 - 50) + b2 * kk*exp(-((kk - b3)^2/b4))
  }else{
    #random overlap
    b1=80.3178;b2=0.6273;b3=-124.8550;b4=23848.2169
    k = 2 * (100/(1 + exp(0 - kk/b1))^1 - 50) + b2 * kk*exp(-((kk - b3)^2/b4))
  }
  return(k)
}
#find crown area index
#' Get total crown area index from aggregate canopy area.
#'
#' @param k Aggregate canopy cover
#' @param avoid TRUE: Crowns compete for light and avoid overlapping; FALSE: Crowns are completely random.
#'
#' @returns summed crown area (crown area index), which may exceed 100%.
#' @export
#'
#' @examples
carea <- function(k, avoid=T){
  #k = canopy cover %
  if(avoid){
    #avoid overlap
    b1 = 23.2610; b2 = 1.1241; b3 = 0.9346
    kk = k * b3 + b1 * (log(100) - log(100 - k))^b2
  }else{
    #random overlap
    b1 = 88.3478; b2 = 1.0353; b3 = 0.2209
    kk = k * b3 + b1 * (log(100) - log(100 - k))^b2
  }
  #component area
  return(kk)
}

#' Estimate number of stems from crown width and canopy cover
#'
#' @param k canopy cover
#' @param cw crown width (m)
#' @param a area (default 1 hectare)
#' @param avoid TRUE: Crowns compete for light and avoid overlapping; FALSE: Crowns are completely random.
#'
#' @returns
#' @export Number of stems per unit area.
#'
#' @examples
nstem <- function(k,cw, a=1, avoid=T){
  #k = canopy cover %
  #crown width m
  #a = area ha
  a0 = 10000*a
  #component area
  kk = carea(k=k, avoid=avoid)
  #relative crown area per unit area
  sa = (cw/2)^2*pi/a0
  #number of stems per unit area
  st = round(kk/sa/100, 0)
  return(st)
}
#find crown width
#' Find crown width from canopy cover and stem density.
#'
#' @param k canopy cover
#' @param st number of stems per unit area
#' @param a area (hectares)
#' @param avoid TRUE: Crowns compete for light and avoid overlapping; FALSE: Crowns are completely random.
#'
#' @returns crown width (meters)
#' @export
#'
#' @examples
findcw <- function(k, st, a=1, avoid=T){
  #k = canopy cover %
  #crown width m
  #a = area ha
  a0 = 10000*a
  #component area
  kk = carea(k=k, avoid=avoid)
  #relative crown area per unit area
  sa = a0*kk/100/st
  #number of stems oer unit area
  cw = round((sa/pi)^0.5*2,1)
  return(cw)
}

#Aggregate cover assuming crowns of same stratum minimally overlap overlap. Parameter 's' 0-1 governs the degree that crowns are allowed to overlap.
cover.agg.stratum <- function(x, s=1/3){
  if (max(x) > 100){warning("Cover cannot exceed 100%!")}else{
    round(pmin(100,sum(x))*(1-s)+100*(1-10^(sum(log10(1-(x/100.0001)))))*s,1)
        }}


# This function re-scales ocular estimates of individual species to be more coherent with the more accurate ocular estimate of total cover for the stratum. Function takes a vector of individual percentages, and a single targeted value of aggregate cover. Used when the number of taxa and stand density makes it a challenge to estimate the absolute cover of each taxon, but for which it is reasonable to estimate their relative proportions within the stand. Values change in proportion to relative cover when total cover is low. But cover values may diverge from relative cover nonlinearly as individual covers approach 100%, because total cover cannot exceed 100%.
# Example:
# taxon.cover = c(10,50,80) #estimated cover for individual taxa in stratum
# aggregate.cover = 80 #estimated cover for whole stratum

rescale.cover <- function(taxon.cover, aggregate.cover){
  if (max(c(taxon.cover,aggregate.cover)) > 100){warning("Cover cannot exceed 100%!")}else{
    cover.est = taxon.cover/100
    cover.total = aggregate.cover/100
    cover.agg1 = 1-10^(sum(log10(1-cover.est)))
    cover.fac1 = (cover.total/cover.agg1)^1.5 #first pass makes a linear adjustment so that relative cover is consistent with field estimate.
    cover.adj1 = (cover.est*cover.fac1)/(max(max(cover.est*cover.fac1),1)+0.01)
    cover.agg2 = 1-10^(sum(log10(1-cover.adj1)))
    cover.fac2 = (log10(1-cover.total)/log10(1-cover.agg2))#second pass fine tunes adjusted cover so that aggregate cover matches ocular total cover.
    cover.agg3 = 1-10^(cover.fac2*log10(1-cover.adj1))
    cover.adj  = cover.agg3*100
    return(cover.adj)}
}


