#Function consolidates recorded plant height data and fills in missing values based on taxon or growth habit depending on what type of height data is missing. This version takes vectors as inputs.

fill.hts <- function(plot = NA_character_ ,
                     taxon = NA_character_ ,
                     type = NA_character_ ,
                     stratum.min = NA_real_,
                     stratum.max = NA_real_,
                     crown.min = NA_real_,
                     crown.max = NA_real_){
  # Ceiling for the taxon
  taxon.max = get.ht.max(taxon)

  # Establish ceiling for the plot, important so that vines and epiphytes do not exceed height of tallest measured vegetation.
  df <- data.frame(plot, taxon, type, stratum.min, stratum.max, crown.min, crown.max, taxon.max) |> as.data.frame()

  df <- df |> group_by(plot) |> mutate(stand.max = pmax(max(stratum.max, na.rm = TRUE, warnings =FALSE), max(crown.max, na.rm = TRUE, warnings =FALSE), na.rm = TRUE, warnings =FALSE),
                                       base.max = pmax(max(stratum.min, na.rm = TRUE, warnings =FALSE), max(crown.min, na.rm = TRUE, warnings =FALSE), na.rm = TRUE, warnings =FALSE ),
                                       stand.max =  ifelse(is.na(stand.max) | base.max >= stand.max, NA_real_, stand.max),
                                       base.max = NULL)
  # Populate plant height for each species within its stratum. Empirical (field measured) "live canopy" heights always get priority over default heights. Membership within a higher stratum can also override taxon default height.
  df <- df |> mutate(ht.max = case_when(
    !is.na(crown.max) ~ crown.max,
    !is.na(stratum.max) ~ pmin(stratum.max, taxon.max),
    TRUE ~ pmin(taxon.max, stand.max)),
    ht.max = case_when(
      !is.na(ht.max) ~ ht.max,
      type %in%  "tree" ~ 24,
      type %in%  "shrub/vine" ~ 3,
      type %in%  "forb" ~ 0.6,
      type %in%  "grass/grasslike" ~ 0.6,
      type %in%  "moss" ~ 0,
      TRUE ~ 0))
  # Establish a default live crown bottom height if not recorded in field.

  df <- df |> mutate(
    ht.max = ifelse(!is.na(stratum.min) & stratum.min >=  ht.max, stratum.min + (stratum.max-stratum.min)/10, ht.max),
    ht.min =  case_when(
      !is.na(crown.min) ~ crown.min,
      TRUE ~ ht.max/2))
  # Round the number of decimal places with higher precision reserved for shorter plants.
  return(data.frame(ht.min = ht.round(df$ht.min),ht.max = ht.round(df$ht.max), stand.max= df$stand.max, taxon.max= df$taxon.max))
}

#Function consolidates recorded plant height data and fills in missing values based on taxon or growth habit depending on what type of height data is missing. This version takes a clean.veg data frame as input.

fill.hts.df <- function(df){

  hts.new <- fill.hts(plot = df$plot ,
                      taxon = df$taxon,
                      type = df$taxon,
                      stratum.min = df$stratum.min,
                      stratum.max = df$stratum.max,
                      crown.min = df$crown.min,
                      crown.max = df$crown.max)

  df$ht.min <- hts.new$ht.min
  df$ht.max <- hts.new$ht.max
  df$taxon.max <- hts.new$taxon.max
  df$stand.max <- hts.new$stand.max

  return(df[,c("plot","taxon","type","nativity","cover","ht.min","ht.max","stratum.min","stratum.max","crown.min","crown.max","diam.min","diam.max","stand.max","taxon.max")])
}
