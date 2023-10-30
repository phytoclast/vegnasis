#This function retains standardized columns, removes non-standard columns, and establishes missing standardized columns with missing data.

pre.fill.veg <- function(x){
  if(!'plot' %in% colnames(x)){x$plot=NA_character_}
  if(!'label' %in% colnames(x)){x$label=NA_character_}
  if(!'date' %in% colnames(x)){x$date=NA}
  if(!'lat' %in% colnames(x)){x$lat=NA_real_}
  if(!'lon' %in% colnames(x)){x$lon=NA_real_}
  if(!'symbol' %in% colnames(x)){x$symbol=NA_character_}
  if(!'taxon' %in% colnames(x)){x$taxon=NA_character_}
  if(!'type' %in% colnames(x)){x$type=NA_character_}
  if(!'habit' %in% colnames(x)){x$habit=NA_character_}
  if(!'nativity' %in% colnames(x)){x$nativity=NA_character_}
  if(!'cover' %in% colnames(x)){x$cover=NA_real_}
  if(!'stratum.min' %in% colnames(x)){x$stratum.min=NA_real_}
  if(!'stratum.max' %in% colnames(x)){x$stratum.max=NA_real_}
  if(!'crown.min' %in% colnames(x)){x$crown.min=NA_real_}
  if(!'crown.max' %in% colnames(x)){x$crown.max=NA_real_}
  if(!'dbh.min' %in% colnames(x)){x$dbh.min=NA_real_}
  if(!'dbh.max' %in% colnames(x)){x$dbh.max=NA_real_}
  if(!'BA' %in% colnames(x)){x$BA=NA_real_}
  if(!'crshape' %in% colnames(x)){x$crshape=NA_character_}
  if(!'crfill' %in% colnames(x)){x$crfill=NA_character_}
  if(!'crcolor' %in% colnames(x)){x$crcolor=NA_character_}
  if(!'stshape' %in% colnames(x)){x$stshape=NA_character_}
  if(!'stfill' %in% colnames(x)){x$stfill=NA_character_}
  if(!'stcolor' %in% colnames(x)){x$stcolor=NA_character_}

    x <- x %>% subset(select= c("plot","label", "date", "lat", "lon","symbol","taxon","type","habit",
                                "nativity","cover","stratum.min","stratum.max","crown.min","crown.max","dbh.min","dbh.max","BA","crshape",
                                "crfill","crcolor","stshape","stfill","stcolor"))
  return(x)
}


