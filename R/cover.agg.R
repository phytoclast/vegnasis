#This function aggregates cover among taxa of a given stratum or guild, assuming random crown distributions, and where totals cannot not exceeding 100%.

cover.agg <- function(x){
  if (max(x) > 100){warning("Cover cannot exceed 100%!")}else{
    round(100*(1-10^(sum(log10(1-(x/100.0001))))),1)}}

#Aggregate cover assuming crowns of same stratum minimally overlap overlap. Parameter 's' 0-1 governs the degree that crowns are allowed to overlap.
cover.agg.stratum <- function(x, s=0.5){
  if (max(x) > 100){warning("Cover cannot exceed 100%!")}else{
    round(pmin(100,sum(x))*(1-s)+100*(1-10^(sum(log10(1-(x/100.0001)))))*s,1)
        }}


# This function re-scales ocular estimates of individual species to be more coherent with the more accurate ocular estimate of total cover for the stratum. Function takes a vector of individual percentages, and a single targeted value of aggregate cover. Used when the number of taxa and stand density makes it a challenge to estimate the absolute cover of each taxon, but for which it is reasonable to estimate their relative proportions within the stand. Values change in proportion to relative cover when total cover is low. But cover values may diverge from relative cover nonlinearly as individual covers approach 100%, because total cover cannot exceed 100%.
# Example:
# taxon.cover = c(10,50,80) #estimated cover for individual taxa in stratum
# aggregate.cover = 80 #estimated cover for whole stratum

rescale.cover <- function(taxon.cover, aggregate.cover){
  if (max(c(taxon.cover,aggregate.cover)) > 100){warning("Cover cannot exceed 100%!")}else{
    cover.est = taxon.cover/100
    cover.total = aggregate.cover/100
    cover.agg1 = 1-10^(sum(log10(1-cover.est)))
    cover.fac1 = (cover.total/cover.agg1)^1.5 #first pass makes a linear adjustment so that relative cover is consistent with field estimate.
    cover.adj1 = (cover.est*cover.fac1)/(max(max(cover.est*cover.fac1),1)+0.01)
    cover.agg2 = 1-10^(sum(log10(1-cover.adj1)))
    cover.fac2 = (log10(1-cover.total)/log10(1-cover.agg2))#second pass fine tunes adjusted cover so that aggregate cover matches ocular total cover.
    cover.agg3 = 1-10^(cover.fac2*log10(1-cover.adj1))
    cover.adj  = cover.agg3*100
    return(cover.adj)}
}


