#convert Text export from VegLog MS Access Database
# library(vegnasis)
# veg.raw <- vegnasis::nasis.veg
# obssites <- vegnasis::obs
# obstaxa <- vegnasis::obsspp
clean.veg.log <- function(obssites, obstaxa){

  obss <- obssites |> mutate(Date = as.Date(ISOdate(year = Year,
                                                    month = Mon,
                                                    day = Day))) |> subset(Year >2001)

  obss <- obss |> select(Observation_ID, Observation_Label,Date,Latitude,Longitude,f3,s3,t2,Lichen_Cover,Moss_Cover,Herb_Cover,Shrub_Cover,TallShrub_Cover,Subcanopy_Cover,Tree_Cover,BA_Factor)
  colnames(obss) <- c("Observation_ID","Observation_Label", "Date","Latitude","Longitude","fieldupper","shrubupper","subcanopyupper", "Lichen", "Bryophyte", "Field.agg", "Shrub.agg","TallShrub.agg","Subcanopy.agg","Tree.agg","BA_Factor")

  obst <- obstaxa |> select(Observation_ID, AcTaxon, Habit, Field, Shrub, TallShrub, Subcanopy, Tree, Fmin,Fmax,Smin,TSmin, TSmax, Smax,SCmin,SCmax,Tmin,Tmax,Dmin,Dmax,BA)



  x <- obss |> left_join(obst, by=c('Observation_ID'='Observation_ID'), multiple='all')
  #determine whether a tall shrub stratum should exist
  x <- x |> group_by(Observation_ID) |> mutate(shrubupper = ifelse(!is.na(TallShrub.agg) | (!is.na(TallShrub) & sum(TallShrub) > 0), 2,5))
  field <- x |> mutate(cover=Field, stratum.min = 0, stratum.max = fieldupper, crown.min = Fmin, crown.max = Fmax, BA = NA_real_, Dmin = NA_real_, Dmax = NA_real_)

  shrub <- x |> mutate(cover=Shrub, stratum.min = fieldupper, stratum.max = shrubupper, crown.min = Smin, crown.max = Smax, BA = NA_real_, Dmin = NA_real_, Dmax = NA_real_)
  tallshrub <- x |> mutate(cover=TallShrub, stratum.min = 2, stratum.max = 5, crown.min = TSmin, crown.max = TSmax, BA = NA_real_, Dmin = NA_real_, Dmax = NA_real_)
  subcan <- x |> mutate(cover=Subcanopy, stratum.min = shrubupper, stratum.max = subcanopyupper, crown.min = SCmin, crown.max = SCmax, BA = ifelse(Tree == 0 & Subcanopy > 0, tree.ct.BA(BA, BA_Factor),NA_real_), Dmin = ifelse(Tree == 0 & Subcanopy > 0, Dmin, NA_real_), Dmax = ifelse(Tree == 0 & Subcanopy > 0, Dmax, NA_real_))
  tree <- x |> mutate(cover=Tree, stratum.min = subcanopyupper, stratum.max = NA_real_, crown.min = Tmin, crown.max = Tmax, BA = ifelse(Tree > 0, tree.ct.BA(BA, BA_Factor),NA_real_), Dmin = ifelse(Tree > 0, Dmin, NA_real_), Dmax = ifelse(Tree > 0, Dmax, NA_real_))

  x <- rbind(field, shrub, tallshrub, subcan, tree) |> select(Observation_ID, Observation_Label, Date, Latitude, Longitude, AcTaxon, cover, stratum.min, stratum.max, crown.min, crown.max, Dmin, Dmax, BA)


  colnames(x) <- c('plot', 'label', 'date', 'lat', 'lon', 'taxon', 'cover', 'stratum.min', 'stratum.max', 'crown.min', 'crown.max', 'dbh.min', 'dbh.max', 'BA')
  x <- x |> mutate(symbol=NA, type=NA, nativity=NA,
                   cover = ifelse(cover > 100, 100, cover),
                   # habit = NA_character_,
                   BA = ifelse(BA <= 0,NA_real_,BA),
  )|>
    subset(cover > 0)|>
    select(plot, label, date, lat, lon, taxon, type, nativity, cover, stratum.min, stratum.max, crown.min, crown.max, dbh.min, dbh.max, BA)
  # x <-  x |> fill.nativity.df() |> fill.type.df() |> mutate(symbol = fill.usda.symbols(taxon))

  return(x)
}
