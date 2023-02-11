#Function takes a data frame of standardize plot data and determines the most dominant species and constructs an empirical association name.

get.assoc <- function(x){
  x <- x |> mutate(stratum = case_when(
    is.na(ht.max) | is.na(type) ~ 'excluded',
    ht.max > 15 & type %in% c('shrub/vine', 'tree')  ~ '4',
    ht.max > 5  & type %in% c('shrub/vine', 'tree')  ~ '3',
    ht.max > 0.5 & type %in% c('shrub/vine', 'tree') ~ '2',
    ht.max > 0 & type %in% c('shrub/vine', 'forb','grass/grasslike') ~ '1',
    !type %in% c('tree', 'shrub/vine', 'forb','grass/grasslike') ~ '0',
    TRUE ~ 'excluded'))


x2 <- x |> subset(!stratum %in% 'excluded')

x2 <- x2 |> group_by(plot, taxon, stratum) |> summarise(cover = cover.agg(cover), ht.min = mean(ht.min), ht.max = mean(ht.max))
x2 <- x2 |> group_by(plot,taxon) |> mutate(maxcover = max(cover), maxht = max(ht.max))
x2 <- x2 |> subset((ht.max == maxht & cover >= 10) | (maxcover < 10 & maxcover == cover))
x2 <- x2 |> group_by(plot,taxon) |> mutate(maxcover = max(cover), maxht = max(ht.max))
x2 <- x2 |> subset((ht.max == maxht & !taxon %in% "" & !is.na(taxon)))

x2 <- x2 |> group_by(plot,stratum) |> mutate(srank = order(order(-cover,-ht.max)))
x2 <- x2 |> group_by(plot) |> mutate(rank = order(order(-cover,-ht.max)))
x2 <- x2 |> group_by(plot, stratum) |> mutate(maxcover = max(cover), maxht = NULL)
x2 <- x2 |> subset(((maxcover == cover & cover >= 10) | rank <=5) & srank <=3)
x2 <- x2 |> group_by(plot) |> mutate(rank = order(order(-as.numeric(stratum), srank)))

associations <- x2 |> subset(select = c(plot)) |> unique()
plots <- associations$plot
associations$association <- ''
for (i in 1:length(plots)){#i=1
  Com.B <- subset(x2, plot %in% plots[i])
  nrank <- length(unique(Com.B$rank))
  assname <- ""
  for (j in 1:nrank){#j=1
    assname <- ifelse(j == 1,Com.B[Com.B$rank %in% j,]$taxon,
                      ifelse(Com.B[Com.B$rank %in% j,]$stratum == Com.B[Com.B$rank %in% (j-1),]$stratum,
                             paste0(assname, '-',Com.B[Com.B$rank %in% j,]$taxon),paste0(assname, '/',Com.B[Com.B$rank %in% j,]$taxon)))

  }
  associations[associations$plot %in% plots[i],]$association <- assname
}

return(associations)
}
