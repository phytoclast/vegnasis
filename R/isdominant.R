#' Determine dominant species
#'
#' @param veg Standardized vegetation data frame
#' @param plot Vector of plot ids (in lieu of data frame)
#' @param taxon Vector of taxon names (in lieu of data frame)
#' @param cover Vector of cover (in lieu of data frame)
#' @param ht.max Vector of maximum plant height (in lieu of data frame)
#' @param type Vector of functional groups (in lieu of data frame)
#' @param append Should add dominant designation as column (TRUE), or show data summarize data frame to 3 strata from which dominance was determined (FALSE)
#'
#' @returns Data frame identifying dominant species as 1, or 0 for non-dominant species in a column called "isdom".
#' @export
#'
#' @examples #create demo data set
#' @examples taxon=c('Tree1', 'Tree2','Tree2', 'Tree3', 'Shrub1', 'Shrub2', 'Shrub3','Tree1', 'Herb1', 'Herb2', 'Herb3')
#' @examples cover=c(10,50,2,1,19,5,14,1,22,15,2)
#' @examples ht.max=c(20,15,6,10,3,2,0.3,2,0.2,0.5,0.1)
#' @examples type = c('tree','tree','tree','tree','shrub/vine','shrub/vine','shrub/vine','tree','forb','forb','grass/grasslike')
#' @examples plot = 'plot1'
#' @examples df <- data.frame(plot=plot,taxon=taxon,cover=cover,ht.max=ht.max,type=type)
#' @examples #input as data frame, appending
#' @examples isdominant(df, append = TRUE)
#' @examples #input as data frame, summarizing strata
#' @examples isdominant(df, append = FALSE)
#' @examples #input as vectors
#' @examples isdominant(plot=plot,taxon=taxon,cover=cover,ht.max=ht.max,type=type)

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
