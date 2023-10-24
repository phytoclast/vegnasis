#function takes a cleaned vegetation plot data fram with plot, taxon, and cover columns, and user specified region, and returns a mean hydric indicator status.

get.wetness <- function(x, region = 'NCNE'){
  x <- x |> group_by(plot, taxon) |> summarise(cover=cover.agg(cover))

  #Lookup default plant hydric indicator status ----
  # region = 'NCNE'
  hydric <- hydric |> mutate(status = case_when(
    region == 'AGCP' ~ AGCP,
    region == 'AW' ~ AW,
    region == 'CB' ~ CB,
    region == 'EMP' ~ EMP,
    region == 'GP' ~ GP,
    region == 'HI' ~ HI,
    region == 'MW' ~ MW,
    region == 'NCNE' ~ NCNE,
    region == 'WMVC' ~ WMVC,
    region == 'AK' ~ AK,
    TRUE ~ other))

  hydric <- hydric |> mutate(
    status = ifelse(is.na(status), rowMeans(select(hydric,
                                                   c('AGCP','AW','CB','EMP','GP','HI','MW','NCNE','WMVC','AK','other')), na.rm = TRUE),status))


  x$status0 = NA_real_
  #first try straight join ----
  x <- x |> left_join(hydric[,c('Scientific.Name','status')], by = c('taxon'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(status0 = ifelse(is.na(status0)| status0 %in% NA_real_, status, status0))
  x <- x[,1:4]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','syn')], by=c('taxon'='syn'), multiple = 'first') |> left_join(hydric[,c('Scientific.Name','status')], by = c('taxon'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(status0 = ifelse(is.na(status0)| status0 %in% NA_real_, status, status0))
  x <- x[,1:4]
  #finally try genus only ----
  #not implemented

  x <- x|> subset(!is.na(status0) & !is.na(cover)) |> group_by(plot) |> summarise(wetness = sum(cover*status0+0.0005)/sum(cover+0.001))
  return(x)
}

#' Get obligate/fac-wet indicator status
#' @description
#' This function gets wetness status using only obligate and faculative-wet species (obligate having twice the weights as fac-wet), and rates all faculative and upland species as zero.\n
#' Army corps regions: 'AGCP' = Atlantic and Gulf Coastal Plain; 'AW' = Arid West; 'CB' = Caribbean; 'EMP' = Eastern Mountains and Piedmont; 'GP' = Great Plains; 'HI' = HI; 'MW' = Midwest; 'NCNE' = Northcentral & Northeast; 'AK' = Alaska; 'WMVC' = Western Mountains, Valleys, and Coast.
#'
#'
#' @param x Species composition data frame cleaned with clean.veg() function.
#' @param region US Army Corps Region where indicator status is prioritized.
#'
#' @return wetness value
#' @export
#'
#' @examples data("nasis.veg")
#' veg <- nasis.veg |> clean.veg()
#' wetland <- veg |> get.obl()

get.obl <- function(x, region = 'NCNE'){
  x <- x |> group_by(plot, taxon) |> summarise(cover=cover.agg(cover))

  #Lookup default plant hydric indicator status ----
  # region = 'NCNE'
  hydric <- hydric |> mutate(status = case_when(
    region == 'AGCP' ~ AGCP,
    region == 'AW' ~ AW,
    region == 'CB' ~ CB,
    region == 'EMP' ~ EMP,
    region == 'GP' ~ GP,
    region == 'HI' ~ HI,
    region == 'MW' ~ MW,
    region == 'NCNE' ~ NCNE,
    region == 'WMVC' ~ WMVC,
    region == 'AK' ~ AK,
    TRUE ~ other))

  hydric <- hydric |> mutate(
    status = ifelse(is.na(status), rowMeans(select(hydric,
                                                   c('AGCP','AW','CB','EMP','GP','HI','MW','NCNE','WMVC','AK','other')), na.rm = TRUE),status),
    status = ifelse(status >= 0.5, (status-0.5)/0.5,0))




  x$status0 = NA_real_
  #first try straight join ----
  x <- x |> left_join(hydric[,c('Scientific.Name','status')], by = c('taxon'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(status0 = ifelse(is.na(status0)| status0 %in% NA_real_, status, status0))
  x <- x[,1:4]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','syn')], by=c('taxon'='syn'), multiple = 'first') |> left_join(hydric[,c('Scientific.Name','status')], by = c('taxon'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(status0 = ifelse(is.na(status0)| status0 %in% NA_real_, status, status0))
  x <- x[,1:4]
  #finally try genus only ----
  #not implemented

  x <- x|> subset(!is.na(status0) & !is.na(cover)) |> group_by(plot) |> summarise(wetness = sum(cover*status0+0.0005)/sum(cover+0.001))
  return(x)
}

#' Get upland/fac-up indicator status
#' @description
#' This function gets upland status using only upland and faculative-upland species (upland having twice the weights as fac-up), and rates all faculative and wetland species as zero.\n
#' Army corps regions: 'AGCP' = Atlantic and Gulf Coastal Plain; 'AW' = Arid West; 'CB' = Caribbean; 'EMP' = Eastern Mountains and Piedmont; 'GP' = Great Plains; 'HI' = HI; 'MW' = Midwest; 'NCNE' = Northcentral & Northeast; 'AK' = Alaska; 'WMVC' = Western Mountains, Valleys, and Coast.
#'
#' @param x Species composition data frame cleaned with clean.veg() function.
#' @param region US Army Corps Region where indicator status is prioritized.
#'
#' @return dryness value
#' @export
#'
#' @examples data("nasis.veg")
#' veg <- nasis.veg |> clean.veg()
#' wetland <- veg |> get.upl()

get.upl <- function(x, region = 'NCNE'){
  x <- x |> group_by(plot, taxon) |> summarise(cover=cover.agg(cover))

  #Lookup default plant hydric indicator status ----
  # region = 'NCNE'
  hydric <- hydric |> mutate(status = case_when(
    region == 'AGCP' ~ AGCP,
    region == 'AW' ~ AW,
    region == 'CB' ~ CB,
    region == 'EMP' ~ EMP,
    region == 'GP' ~ GP,
    region == 'HI' ~ HI,
    region == 'MW' ~ MW,
    region == 'NCNE' ~ NCNE,
    region == 'WMVC' ~ WMVC,
    region == 'AK' ~ AK,
    TRUE ~ other))

  hydric <- hydric |> mutate(
    status = ifelse(is.na(status), rowMeans(select(hydric,
                                                   c('AGCP','AW','CB','EMP','GP','HI','MW','NCNE','WMVC','AK','other')), na.rm = TRUE),status),status = 1-status,
    status = ifelse(status >= 0.5, (status-0.5)/0.5,0))

  x$status0 = NA_real_
  #first try straight join ----
  x <- x |> left_join(hydric[,c('Scientific.Name','status')], by = c('taxon'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(status0 = ifelse(is.na(status0)| status0 %in% NA_real_, status, status0))
  x <- x[,1:4]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','syn')], by=c('taxon'='syn'), multiple = 'first') |> left_join(hydric[,c('Scientific.Name','status')], by = c('taxon'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(status0 = ifelse(is.na(status0)| status0 %in% NA_real_, status, status0))
  x <- x[,1:4]
  #finally try genus only ----
  #not implemented

  x <- x|> subset(!is.na(status0) & !is.na(cover)) |> group_by(plot) |> summarise(dryness = sum(cover*status0+0.0005)/sum(cover+0.001))
  return(x)
}

