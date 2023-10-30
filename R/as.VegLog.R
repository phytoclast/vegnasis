#create new class

setClass("VegLog",
         slots = c(
           site = "data.frame",
           plants = "data.frame"
         )
)


#' Coerce existing data frame to VegLog object.
#'
#' @param x data frame containing standard vegetation columns
#'
#' @return VegLog data containing slots for site data and plant data
#' @export
#'
#' @examples obssites <- vegnasis::obs
#' obstaxa <- vegnasis::obsspp
#' veg=clean.veg.log(obssites, obstaxa)
#' manyplots = as.VegLog(veg)

as.VegLog <- function(x){
  x = pre.fill.veg(x)
  site <- unique(subset(x, select=c("plot","label","date","lat","lon")))
  plants = subset(x, select=c("plot","symbol","taxon","type","nativity","cover","stratum.min","stratum.max","crown.min","crown.max","dbh.min","dbh.max","BA"))

  set = new("VegLog", site = site, plants = plants)
  return(set)}


#' Merge several VegPlot objects
#'
#' @param VegLog one or more VegLog objects
#'
#' @return new larger VegLog object
#' @export
#'
#' @examples obssites <- vegnasis::obs
#' obstaxa <- vegnasis::obsspp
#' veg=clean.veg.log(obssites, obstaxa)
#' veg1 = as.VegLog(veg[1:10,])
#' veg2 = as.VegLog(veg[50:60,])
#' veg3 = c(veg1,veg2)

setMethod("c", signature = "VegLog",
          function(x,...){
            x.list <- list(x,...)
            n.list <- length(x.list)
            for(i in 1:n.list){
              site0 = x.list[[i]]@site
              plants0 = x.list[[i]]@plants
              if(i == 1) {
                site = site0
                plants = plants0}else{
                  site = rbind(site,site0)
                  plants =  rbind(plants,plants0)}}
            newset <- new("VegLog", site = site, plants = plants)
            return(newset)})
