library(soilDB)
library(vegnasis)
library(aqp)
library(sf)
library(mapview)

pedons20251212 <- readRDS('data_raw/pedons20251212.RDS')
usethis::use_data(pedons20251212, overwrite = T)

veg.raw <- vegnasis::veg.raw20250414
plots.raw <- vegnasis::vegplot20250414
colnames(plots.raw)
myplots <- plots.raw |> subset(grepl('Greg.Schmidt',primarydatacollector))
veg <- veg.raw |> subset(vegplotid %in% myplots$vegplotid) |> clean.veg() |> fill.hts.df() |> fill.hts.df()

veg2 <- veg |>
  mutate(stratum = case_when(ht.max > 5 ~ 'tree',
                             ht.max > 0.5 & type %in% c('tree','shrub/vine') ~ 'shrub',
                             TRUE ~ 'herb')) |>
  group_by(plot, stratum, taxon) |> summarise(ht.max = weighted.mean(ht.max,cover), ht.min = weighted.mean(ht.min,cover), cover = cover.agg(cover), type=first(type))

veg2 <- veg2 |> group_by(plot, stratum) |> mutate(stratumcover = cover.agg(cover), stratumsum = sum(cover),  nsp = length(cover)) |> ungroup()


veg2 <- veg2 |> arrange(plot, stratum, -cover) |> group_by(plot, stratum) |> mutate(rcover = (cover)/stratumsum,cumtotal = cumsum(cover)/stratumsum, is50 = ifelse(cumtotal >=0.5,1,0), cumis50 =cumsum(is50), rnk = rank(-rcover, ties.method = 'first')) |> ungroup() |> mutate(isdom = ifelse(stratumcover >= 5 & (cumis50 <= 1 | cover >= 20),1,0)) |> subset(stratumsum >0)

domspp <- veg2 |>  group_by(plot, stratum) |> summarise(ndom = sum(isdom))

sumdomspp <- domspp  |>   group_by(stratum) |>   summarise(mindom = min(ndom, na.rm = T), meandom = mean(ndom, na.rm = T), maxdom = max(ndom, na.rm = T))

mcover <- veg2 |> group_by(rnk) |> summarise(mcov = mean(rcover))

ddrop <- veg2 |> subset(cumis50 %in% c(1,2)) |>  group_by(plot, stratum) |> summarise(mxcov = max(rcover), mncov = min(rcover), ddrp = mncov/mxcov) |> ungroup() |> summarise(ddrp2 = mean(ddrp))


#####attempt function ----
taxon=veg$taxon
cover=veg$cover
ht.max=veg$ht.max
type = veg$type
veg <- veg
isdominant <-  function(veg=NULL, taxon, cover, ht.max, type){
  if(is.null(veg)){
    veg <- data.frame(taxon=taxon,cover=cover, ht.max=ht.max, type=type)
  }

  group_taxa <- c(group_vars(veg),'stratum','taxon')
  group_strat <- c(group_vars(veg),'stratum')
  group_arrange <- c(group_vars(veg),'stratum','-cover')
  joincol <- c(group_vars(veg),"stratum","taxon","isdom")

  veg <- veg |>
    mutate(stratum = case_when(ht.max > 5 ~ 'tree',
                               ht.max > 0.5 & type %in% c('tree','shrub/vine') ~ 'shrub',
                               TRUE ~ 'herb'))
  veg2 <- veg |>
    group_by(across(all_of(group_taxa))) |> summarise(ht.max = weighted.mean(ht.max,cover), cover = cover.agg(cover), type=first(type)) |> ungroup()

  veg2 <- veg2 |> group_by(across(all_of(group_strat))) |> mutate(stratumcover = cover.agg(cover), stratumsum = sum(cover),  nsp = length(cover)) |> ungroup()

  veg2 <- veg2 |> group_by(across(all_of(group_strat))) |> arrange(-cover) |> mutate(rcover = (cover)/stratumsum, cumtotal = cumsum(cover)/stratumsum, is50 = ifelse(cumtotal >=0.5,1,0), cumis50 =cumsum(is50), rnk = rank(-rcover, ties.method = 'first')) |> ungroup() |> mutate(isdom = ifelse(stratumcover >= 5 & ((cumis50 <= 1 & rnk <=10) | (cover >= 20 & rnk <=5)),1,0))

  veg <- veg |> left_join(veg2[,joincol])
  return(veg$isdom)

}




veg <- veg.raw |> subset(vegplotid %in% myplots$vegplotid) |> clean.veg() |> fill.hts.df() |> fill.hts.df()

veg <- veg |> group_by(plot) |> mutate(dom = isdominant(veg))
