#This function fill in missing nativity status based on user input of region. If no region is specified, the default is native to the United States.
fill.nativity <- function(taxa,  region=NA, nativity=NA){
  x  <-  data.frame(taxa=taxa, nativity = nativity)
  #first try straight join ----
  x <- x |> left_join(natdat, by = c('taxa'='ac.binomial'), multiple = 'first')
  x <- x |> mutate(nativity0 = case_when(
    region %in% 'Northwest' ~ Northwest,
    region %in% 'Southwest' ~ Southwest,
    region %in% 'NorthCentral' ~ NorthCentral,
    region %in% 'Southcentral' ~ Southcentral,
    region %in% 'Northeast' ~ Northeast,
    region %in% 'Southeast' ~ Southeast,
    region %in% 'Alaska' ~ Alaska,
    region %in% 'Hawaii' ~ Hawaii,
    region %in% 'Caribbean' ~ Caribbean,
    region %in% 'CanadaWest' ~ CanadaWest,
    region %in% 'CanadaEast' ~ CanadaEast,
    region %in% 'Arctic' ~ Arctic,
    region %in% 'Mexico' ~ Mexico,
    TRUE ~ Northwest+Southwest+NorthCentral+Southcentral+Northeast+Southeast+Alaska+Hawaii+Caribbean))
  x <- x |> mutate(nativity = ifelse(is.na(nativity), ifelse(nativity0 > 0,'native','introduced'), nativity))
  x <- x[,1:2]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','ac.binomial','syn')], by=c('taxa'='syn'), multiple = 'first') |> left_join(natdat, by = c('ac.binomial'='ac.binomial'), multiple = 'first')
  x <- x |> mutate(nativity0 = case_when(
    region %in% 'Northwest' ~ Northwest,
    region %in% 'Southwest' ~ Southwest,
    region %in% 'NorthCentral' ~ NorthCentral,
    region %in% 'Southcentral' ~ Southcentral,
    region %in% 'Northeast' ~ Northeast,
    region %in% 'Southeast' ~ Southeast,
    region %in% 'Alaska' ~ Alaska,
    region %in% 'Hawaii' ~ Hawaii,
    region %in% 'Caribbean' ~ Caribbean,
    region %in% 'CanadaWest' ~ CanadaWest,
    region %in% 'CanadaEast' ~ CanadaEast,
    region %in% 'Arctic' ~ Arctic,
    region %in% 'Mexico' ~ Mexico,
    TRUE ~ Northwest+Southwest+NorthCentral+Southcentral+Northeast+Southeast+Alaska+Hawaii+Caribbean))
  x <- x |> mutate(nativity = ifelse(is.na(nativity), ifelse(nativity0 > 0,'native','introduced'), nativity))
  x <- x[,1:2]
  return(x$nativity)}
