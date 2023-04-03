#This function retains standardized columns, removes non-standard columns, and establishes missing standardized columns with missing data.

pre.fill.veg <- function(x){
  if(!'plot' %in% colnames(x)){x$plot=NA_character_}
  if(!'label' %in% colnames(x)){x$label=NA_character_}
  if(!'symbol' %in% colnames(x)){x$symbol=NA_character_}
  if(!'taxon' %in% colnames(x)){x$taxon=NA_character_}
  if(!'type' %in% colnames(x)){x$type=NA_character_}
  if(!'nativity' %in% colnames(x)){x$nativity=NA_character_}
  if(!'cover' %in% colnames(x)){x$cover=NA_real_}
  if(!'stratum.min' %in% colnames(x)){x$stratum.min=NA_real_}
  if(!'stratum.max' %in% colnames(x)){x$stratum.max=NA_real_}
  if(!'crown.min' %in% colnames(x)){x$crown.min=NA_real_}
  if(!'crown.max' %in% colnames(x)){x$crown.max=NA_real_}
  if(!'dbh.min' %in% colnames(x)){x$dbh.min=NA_real_}
  if(!'dbh.max' %in% colnames(x)){x$dbh.max=NA_real_}
  if(!'BA' %in% colnames(x)){x$BA=NA_real_}

    x <- x %>% subset(select= c("plot","label","symbol","taxon","type",
                                "nativity","cover","stratum.min","stratum.max","crown.min","crown.max","dbh.min","dbh.max","BA"))
  return(x)
}


