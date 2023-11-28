#This function summarizes cover by stratum membership and growth habit. Inputs require a data frame processed by clean.veg and  user supplied vector of stratum height breaks. ----

summary.strata <-  function(x, breaks=c(0.5,5,15)){
  y <- NULL
  nbks <- length(breaks)+1
  brks <- c(0,breaks,1000)
  for(i in 1:(nbks)){#i = 8
    y0 <- x %>% subset(ht.max <= brks[i+1] & ht.max > brks[i])

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
    y0 <- x %>% subset(ht.min <= brks[i+1] & ht.max > brks[i])

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


#Function summarizes a set of plot by max and minimum cover and cover weighted height.
#' Aggregate Summary of Cover by Stratum for Multiple Relevé Plots
#'
#'Function summarizes a set of multiple plots by max and minimum cover and cover weighted height. Used for populating vegetation tables in NRCS EDIT database (formerly ESIS).
#'
#' @param x Species composition data frame with standardized height, cover, and growth habit columns.
#' @param group Optional string identifying column used to group plots.
#' @param breaks Vector of user defined stratum breaks.
#' @param lowerQ Lower percentile for cover (proportion 0-1, not percentage).
#' @param upperQ Upper percentile for cover (proportion 0-1, not percentage).
#' @param woodytypes A vector of woody habit "types" which will be treated among multiple strata. Others not listed will be maintained in the lowest stratum regardless of plant height.
#'
#' @return Data frame listing taxa in multiple rows by stratum and multiple measures of summarized abundances. Summary value column definitions:\cr
#'  \code{Bottom}=  Mean of canopy bottom height weighted by cover.mean.\cr
#'  \code{Top}=  Mean of canopy top height weighted by cover.mean.\cr
#'  \code{cover.Low}= The lower quantile of canopy cover of the taxon within this stratum, considering absences from a given plot as zeros.\cr
#'  \code{cover.High}= The upper quantile of canopy cover of the taxon within this stratum, considering absences from a given plot as zeros.\cr
#'  \code{cover.mean}= The mean canopy cover of the taxon within this stratum, considering absences from a given plot as zeros.\cr
#'  \code{cover.pp}= The mean canopy cover of the taxon within this stratum, when present in plot, considering strata where absent as zeros, but ignoring from calculation if absent from every stratum in a plot.\cr
#'  \code{cover.ps}= The mean canopy cover of the taxon within this stratum, when present in the stratum (ignoring strata where absent).\cr
#'  \code{frq.plot}= Proportion of plots that taxon occurs in.\cr
#'  \code{frq.strat}= Proportion of strata that the taxon occurs in.\cr
#'  \code{dbh.low}= Mean of DBH low values weighted by cover.mean.\cr
#'  \code{dbh.high}= Mean of DBH high values weighted by cover.mean.\cr
#'  \code{BA.Low}= Estimated lower quantile basal area of taxon/stratum (redistributed like BA.mean, see below).\cr
#'  \code{BA.High}= Estimated upper quantile basal area of taxon/stratum (redistributed like BA.mean, see below).\cr
#'  \code{BA.mean}= Estimated mean basal area of taxon/stratum based on total plot basal area redistributed according to the cover.mean of each taxon/stratum. \cr
#'
#'
#' @export
#'
#' @examples x.spp <- soilDB::get_vegplot_species_from_NASIS_db()
#' @examples x.cleaned <- clean.veg(x.spp)
#' @examples x.filled <- fill.hts.df(x.cleaned)
#' @examples x.ESIS <- summary.ESIS(x.filled, breaks = c(0.5, 5, 12))
#'
summary.ESIS <-  function(x, group = NA, breaks=c(0.5,5,15), lowerQ=0.25, upperQ=0.75,woodytypes = c('tree','shrub/vine', 'epiphyte')){
  x = as.data.frame(x) |> mutate(dbh.min= ifelse(is.na(dbh.min), dbh.max,dbh.min))
  if(is.na(group)){x$group <- 1}else{
    x$group <- x[,group]}
  #frequency of whole plot
  f <- x |> subset(cover>0, select=c(plot,group,taxon)) |> unique()  |> mutate(freq=1)
  getnplots = subset(f, select=c(group,plot)) |> unique() |> group_by(group) |> summarise(nplots = length(plot))
  f <- f  |> group_by(taxon, group) |> summarise(freq=sum(freq)) |>
    left_join(getnplots, by=join_by(group==group)) |> mutate(frq.plot=freq/nplots)

  y <- NULL
  nbks <- length(breaks)+1
  brks <- c(0,breaks,1000)

  #extract means by stratum and plot
  for(i in 1:(nbks)){#i = 1
    y0 <- x |> subset((ht.max <= brks[i+1] & ht.max > brks[i] & type %in% woodytypes)|(i==1 & !type %in% woodytypes))

    if(nrow(y0)>0){
      y0 <- y0 %>% mutate(stratum=i, stratum.label = paste0(brks[i], ifelse(i==nbks, "+",paste0("-", brks[i+1]))), ht.min= ht.min, ht.max = ht.max, stratum.min = brks[i], stratum.max = brks[i+1])
      y1 <- y0 %>% group_by(plot, group, symbol, taxon, type, stratum, stratum.label, stratum.min, stratum.max) %>%
        summarise(Cover = cover.agg(cover),
                  ht.min=weighted.mean(ht.min, cover+0.001, na.rm=TRUE),
                  ht.max=weighted.mean(ht.max, cover+0.001, na.rm=TRUE),
                  dbh.min =  weighted.mean(dbh.min, cover+0.001, na.rm=TRUE),
                  dbh.max =  weighted.mean(dbh.max, cover+0.001, na.rm=TRUE),
                  dbh.min =  ifelse(is.nan(dbh.min), NA, dbh.min),
                  dbh.max =  ifelse(is.nan(dbh.max), NA, dbh.max),
                  BA =  sum(BA, na.rm=TRUE))

      if(is.null(y)){y <- y1}else{y <- rbind(y, y1)}}
  }
  #weighted mean of heights of each taxon stratum among all plots
  y = y |> group_by(group, symbol,taxon,type,stratum, stratum.label,stratum.min, stratum.max) |>
    mutate(Bottom=round(weighted.mean(ht.min, Cover+0.001, na.rm=TRUE),1),
           Top=round(weighted.mean(ht.max, Cover+0.001, na.rm=TRUE),1),
           dbh.Low =  weighted.mean(dbh.min, Cover+0.001, na.rm=TRUE),
           dbh.High =  weighted.mean(dbh.max, Cover+0.001, na.rm=TRUE),
           dbh.Low =  ifelse(is.nan(dbh.Low), NA, round(dbh.Low,0)),
           dbh.High =  ifelse(is.nan(dbh.High), NA, round(dbh.High,0)),
           cover.ps = round(mean(Cover),1))

  y = y |> group_by(group, plot) |> mutate(totalBA = sum(BA, na.rm = TRUE), overCover = ifelse(Top > 5, Cover, NA), grossCover = sum(overCover, na.rm = TRUE), BA = round(totalBA*Cover/(grossCover+0.000001),1))
  #get frequency in stratum
  y = y |> group_by(group, plot,symbol,taxon,type) |> mutate(frq.strat = ifelse(sum(Cover)>0,1,0))
  #insert zeros for missing species found in other plots
  y.plot <- unique(subset(y, select=c("group", "plot")))
  y.mid <- unique(subset(y, select=c("group","symbol","taxon","type","stratum","stratum.label","stratum.min", "stratum.max","Bottom","Top","dbh.Low","dbh.High","cover.ps")))
  y.fill <- merge(y.plot, y.mid, by='group') |> mutate(Cover = 0, BA = 0, frq.strat=0)
  y.fill <- rbind(y, y.fill)
  y.fill <- y.fill |> group_by(plot,group, symbol,taxon,type,stratum,stratum.label, stratum.min, stratum.max,Bottom,Top,dbh.Low, dbh.High, cover.ps) |> summarise(Cover = max(Cover), BA = max(BA), frq.strat=max(frq.strat))
  #get quantiles in consideration of zeros for absences
  y.fill <- y.fill |> group_by(group, taxon, symbol,type, stratum, stratum.label, stratum.min, stratum.max, Bottom, Top, dbh.Low, dbh.High, cover.ps) |>
    summarise(cover.Low = round(quantile(Cover, lowerQ),1),
              cover.mean = mean(Cover),
              cover.High = round(quantile(Cover, upperQ),1),
              BA.Low = round(quantile(BA, lowerQ),1),
              BA.mean = mean(BA),
              BA.High = round(quantile(BA, upperQ),1),
              frq.strat = round(mean(frq.strat),3))
  y.fill <- y.fill |> group_by(group, symbol, taxon, type) |> mutate(taxon.cover = cover.agg(cover.mean), over.cover = cover.agg(ifelse(Top > 5,cover.mean,0)))
  y.fill <- y.fill |> group_by(group, type) |> mutate(type.top = max(Top))
  y.fill <- left_join(y.fill, f) |> mutate(BA.pp = round(BA.mean/frq.plot,1),
                                           cover.pp = round(cover.mean/frq.plot,1),
                                           BA.mean = round(BA.mean,1),
                                           cover.mean = round(cover.mean,1),
                                           frq.plot = round(frq.plot,3),
                                           stratum.max = ifelse(stratum.max > (floor((max(Top)+5)/5)*5),(floor((max(Top)+5)/5)*5),stratum.max))

  y.fill <- subset(y.fill, select=c(group, taxon, symbol, type, stratum, stratum.label, stratum.min, stratum.max, Bottom,Top, cover.Low, cover.High, cover.mean, cover.pp, cover.ps, frq.plot, frq.strat, dbh.Low, dbh.High, BA.Low, BA.High, BA.mean, BA.pp, taxon.cover,over.cover, type.top)) |> arrange(-type.top, type, -over.cover, -taxon.cover, -Top)


  return(y.fill)
}

#Function takes results of ESIS summary and flattens record to single row per taxon, with mean cover by stratum columns.

#' Flatten a summary of multiple relevé vegetation plots.
#'
#' @param veg.summ Data frame results from summary.ESIS() function.
#' @param breaks Specify original stratum breaks if input data is missing strata (e.g. breaks = c(0.5, 2, 5, 12)).
#'
#' @return Data frame with the multiple strata as row converted to columns and displaying each taxon in a single row.
#' @export
#'
#' @examples veg <- vegnasis::nasis.veg |> clean.veg() #Get example data.
#' @examples veg <- subset(veg, plot %in% unique(veg$plot)[1:5]) |> fill.hts.df() #Get arbitrary subset of example data.
#' @examples veg.summ <-  summary.ESIS(veg) #Mutliple relevé summary.
#' @examples veg.flat <- flat.summary(veg.summ) #Flatten multiple strata rows to columns.
#' @examples # write.csv(veg.flat, 'veg.flat.csv', row.names = FALSE) #Save as spreadsheet to share.

flat.summary <- function(veg.summ, breaks=NULL){
  if(is.null(breaks))
  {breaks <- sort(unique(veg.summ$stratum.min))
  breaks <- breaks[breaks>0]}
  nbks <- length(breaks)+1
  brks <- c(0,breaks,1000)
  veg.con <- veg.summ |> group_by(taxon,symbol,type,frq.plot, taxon.cover,over.cover,type.top) |> summarise(
    ht.max = max(Top))
  for(i in 1:nbks){#i=6
    y <- veg.summ |> group_by(taxon,symbol,type,frq.plot, taxon.cover,over.cover,type.top) |>
      summarise(c0 = sum(ifelse(stratum %in% i, cover.mean,0))) |> mutate(c0 = ifelse(c0 > 0, c0, NA_real_))
    veg.con <- veg.con |> cbind(c0=y$c0)
    colnames(veg.con)[colnames(veg.con) %in% 'c0'] <- paste0('c',i)
    cname <- paste0('c',i)
    if(i==1){cnames=cname}else{cnames=c(cnames,cname)}

    altname <- case_when(i < nbks ~ paste0(brks[i],'-',brks[i+1],' m'),
                         TRUE ~ paste0(brks[i],'+ m'))
    if(i==1){altnames=altname}else{altnames=c(altnames,altname)}
  }
  veg.con <- veg.con |>
    arrange(-type.top, type, -over.cover, -taxon.cover, -ht.max, taxon) |> subset(select = c('symbol','taxon','type','frq.plot',cnames,'ht.max','over.cover','taxon.cover'))
  colnames(veg.con)[colnames(veg.con) %in% c(cnames,'frq.plot')] <- c('frq',altnames)
  return(veg.con)}
