#This function determines the percentage cover consisting of flora introduced to the region since 1492.
exo.flora <-  function(x){
x <- x |> group_by(plot, taxon, nativity)|> summarise(cover = cover.agg(cover))
x <- x |> mutate(allcover = ifelse(nativity %in% c('native', 'introduced'), cover, NA_real_),
                 introcover = ifelse(nativity %in% c('introduced'), cover, NA_real_))
x <- x |> group_by(plot)|> summarise(introduced = 100*sum(introcover, na.rm = TRUE)/(sum(allcover, na.rm = TRUE)+0.001))
return(x)}
