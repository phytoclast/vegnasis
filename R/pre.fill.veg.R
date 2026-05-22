#This function retains standardized columns, removes non-standard columns, and establishes missing standardized columns with missing data.

#' Fill in missing columns with blanks
#'
#' Several vegnasis functions require data frames with a predefined standard set of column names to operate. This function processes a data frame, retaining existing standardized columns, removing non-standard columns, and establishing missing standardized columns with missing data. Prior to using this function, steps are initially taken to manually assign existing data to standardized column names as much as possible, whereas this function builds the remaining missing structure to allow subsequent vegnasis functions to operate. Units of measurement are assumed to be SI for the purposes of analysis and compatibility with vegetation science literature. Conversions to USC units, if desired, should take place only as a final step after analysis.
#'
#' "plot" = Unique identifer for plot.
#'
#' "label" = Convenient contextual name for plot.
#'
#' "symbol"= USDA PLANTS symbol.
#'
#' "taxon" = Species (binomial), and sometimes genus, subspecies, or varieties.
#'
#' "type" = Growth habit or form such as 'tree', 'shrub/vine', 'grass/grasslike', 'forb', 'moss', etc.
#'
#' "habit" = More detailed life form/growth habit involving standardized code optionally filled by get.habit.code() function.
#'
#' "nativity" = 'native' or 'introduced' status within the geographic area or the plot.
#'
#' "cover" = crown or canopy cover (not the same as foliar cover).
#'
#' "stratum.min" = minimum height range (m) of stratum considered to be occupied by this taxon.
#'
#' "stratum.max" = maximum height range (m) of stratum considered to be occupied by this taxon.

#' "crown.min" = bottom height (m) of live canopy for members of this taxon occupying this stratum.
#'
#' "crown.max" = top height (m) of live canopy for members of this taxon occupying this stratum.
#'
#' "dbh.min" = mimimum diameter (cm) of trees measured (usually at "breast height", 1.37 or 1.4 m above the grown, a.k.a. 'DBH').
#'
#' "dbh.max"= maximum diameter (cm) of trees measured (usually at "breast height", 1.37 or 1.4 m above the grown, a.k.a. 'DBH').
#'
#' "BA" = basal area (square meters per hectare).
#'
#' "crshape" = Optional user defined crown shape name (catalog under development).
#'
#' "crfill" = Optional user defined color for crown fill as color name or hexcode.
#'
#' "crcolor" = Optional user defined color for crown outline as color name or hexcode.
#'
#' "stshape" = Optional user defined stem shape name (catalog under development).
#'
#' "stfill" = Optional user defined color for stem fill as color name or hexcode.
#'
#' "stcolor" = Optional user defined color for stem outline as color name or hexcode.
#'
#' "cw" = Optional user defined crown width (m).
#'
#' @param x User developed data frame with a variable number standard and non-standard column names.
#' @param include Additional columns to retain from original data frame.
#' @param exclude Exclude columns (crshape, crfill, crcolor, stshape, stfill, stcolor, cw) used to generate diagrams (T/F).
#'
#' @returns Vegetation data frame with required columns.
#' @export
#'
#' @examples #Example data created to look as if imported from random csv file.
#' @examples obsite <- c('plot1','plot1','plot1', 'plot2', 'plot2')
#' @examples obsspp <- c('Acer rubrum','Pinus strobus','Pteridium aquilinum', 'Lindera benzoin', 'Trillium grandiflorum')
#' @examples abund <- c(80,10,30,10,10)
#' @examples mydata <- data.frame(obsite=obsite, obsspp=obsspp, abund=abund)
#' @examples
#' @examples #Identify columns containing data corresponding to standard column names.
#' @examples mydata <- mydata |> mutate(taxon=obsspp, cover=abund, plot=obsite)
#' @examples veg <- mydata |> pre.fill.veg()
pre.fill.veg <- function(x, include=NA, exclude=FALSE){
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
  if(!'cw' %in% colnames(x)){x$cw=NA_real_}
  basecols <- c("plot","label", "date", "lat", "lon","symbol","taxon","type","habit",
                "nativity","cover","stratum.min","stratum.max","crown.min","crown.max","dbh.min","dbh.max","BA","crshape","crfill","crcolor","stshape","stfill","stcolor","cw")
  finalcols <- unique(c(basecols, include))
  if(exclude){finalcols <- finalcols[!finalcols %in% c('crshape', 'crfill', 'crcolor', 'stshape', 'stfill', 'stcolor', 'cw')]}
  finalcols <- finalcols[!is.na(finalcols)]
    x <- x %>% subset(select=finalcols)
  return(x)
}


