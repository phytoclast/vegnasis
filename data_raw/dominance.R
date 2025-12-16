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
plot = veg$plot
veg <- veg

isdominant <-  function(veg=NULL, plot, taxon, cover, ht.max, type, append=FALSE){
  if(is.null(veg)){
    veg <- data.frame(plot=plot, taxon=taxon,cover=cover, ht.max=ht.max, type=type)
  }
  inputcols <- colnames(veg)
  group_taxa <- c('plot','stratum','taxon')
  group_strat <- c('plot','stratum')
  joincol <- c('plot',"stratum","taxon","isdom")

  veg <- veg |>
    mutate(stratum = case_when(ht.max > 5 ~ 'tree',
                               ht.max > 0.5 & type %in% c('tree','shrub/vine') ~ 'shrub',
                               TRUE ~ 'herb'))
  veg2 <- veg |>
    group_by(across(all_of(group_taxa))) |> summarise(ht.max = weighted.mean(ht.max,cover), cover = cover.agg(cover), type=first(type)) |> ungroup()

  veg2 <- veg2 |> group_by(across(all_of(group_strat))) |> mutate(stratumcover = cover.agg(cover), stratumsum = sum(cover),  nsp = length(cover)) |> ungroup()

  veg2 <- veg2 |> group_by(across(all_of(group_strat))) |> arrange(desc(cover), by_group = TRUE) |> mutate(rcover = (cover)/stratumsum, cumtotal = cumsum(cover)/stratumsum, is50 = ifelse(cumtotal >=0.5,1,0), cumis50 =cumsum(is50), rnk = rank(-rcover, ties.method = 'first')) |> ungroup() |> mutate(isdom = ifelse(stratumcover >= 5 & ((cumis50 <= 1 & rnk <=10) | (cover >= 20 & rnk <=5)),1,0))

  if(append){veg <- veg |> left_join(veg2[,joincol]) |> subset(select = c(inputcols, 'isdom'))
  }else{
    veg <- veg2 |> arrange(plot, desc(stratum),desc(cover)) |> subset(select = c('plot','taxon','cover','stratum','ht.max', 'isdom'))
  }

  return(veg)
}

#create demo data set
taxon=c('Tree1', 'Tree2','Tree2', 'Tree3', 'Shrub1', 'Shrub2', 'Shrub3','Tree1', 'Herb1', 'Herb2', 'Herb3')
cover=c(10,50,2,1,19,5,14,1,22,15,2)
ht.max=c(20,15,6,10,3,2,0.3,2,0.2,0.5,0.1)
type = c('tree','tree','tree','tree','shrub/vine','shrub/vine','shrub/vine','tree','forb','forb','grass/grasslike')
plot = 'plot1'
df <- data.frame(plot=plot,taxon=taxon,cover=cover,ht.max=ht.max,type=type)
#input as data frame, appending
isdominant(df, append = TRUE)
#input as data frame, summarizing strata
isdominant(df, append = FALSE)
#input as vectors
isdominant(plot=plot,taxon=taxon,cover=cover,ht.max=ht.max,type=type)



veg=df
