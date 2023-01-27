#This function aggregates cover among species within the same stratum or guild assuming random crown distributions, and not exceeding 100%.

cover.agg <- function(x){round(100*(1-10^(sum(log10(1-(x/100.001))))),1)}

