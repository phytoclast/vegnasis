#upgrade taxonomy ----
#This function synonymizes taxa with BONAP or Kew's Plants of the World Online circa 2022.
#' Harmonize taxa
#' @description
#' This function synonymizes taxa with BONAP (Biota of North America Program), Kew's Plants of the World Online (POW) circa 2023, or USDA's PLANTS database.  This ensures that aggregating functions work off a shared taxonomic backbone. An option exists to replace commonly misapplied Old World binomials: Equisetum hyemale, Athyrium filix-femina, Cypripedium calceolus, and Osmunda regalis, with: Equisetum praealtum, Athyrium angustum, Cypripedium parviflorum, and Osmunda spectabilis, respectively.
#'
#' @param taxa Plant name as recorded in original plot data
#' @param fix Optional fix for commonly misapplied Old World binomials to ensure they are assigned to North American species before synonymizing.
#' @param sensu Which taxonomy to follow for accepted names (options: BONAP = 'bonap'; Kew's POW = 'kew'; USDA PLANTS = 'usda')
#'
#' @return Accepted name
#' @export
#'
#' @examples
#' veg <- vegnasis::nasis.veg |> clean.veg()
#' veg$taxon <- harmonize.taxa(veg$taxon)
#' taxa = c('Trientalis borealis', 'Athyrium filix-femina')
#' taxa = harmonize.taxa(taxa, fix = TRUE, sensu = 'bonap'); taxa
#' taxa = harmonize.taxa(taxa, fix = TRUE, sensu = 'usda'); taxa
harmonize.taxa <- function(taxa, fix=FALSE, sensu = 'bonap'){
  x  <-  data.frame(taxa=taxa)
  if(fix){
    fixtaxa <- data.frame(america=c('Cornus sericea', 'Equisetum praealtum', 'Athyrium angustum', 'Cypripedium parviflorum' , 'Osmunda spectabilis'), auctnon=c('Cornus alba','Equisetum hyemale', 'Athyrium filix-femina', 'Cypripedium calceolus', 'Osmunda regalis'))
    x <- x |> left_join(fixtaxa, by=c('taxa'='auctnon'), multiple = 'first')
    x <- x |> mutate(taxa = ifelse(is.na(america), taxa, america))
  }
  x <- x |> left_join(syns2, by = join_by(taxa==taxon), multiple = 'first')

  if(sensu %in% 'kew'){
    x <- x |> mutate(return = ifelse(is.na(kew), taxa, kew))
  }else if(sensu %in% 'usda'){
    x <- x |> mutate(return = ifelse(is.na(usda), taxa, usda))
  }else{
    x <- x |> mutate(return = ifelse(is.na(bonap), taxa, bonap))}
  return(x$return)}

#fill USDA PLANTS Symbols ----
#This function fills in missing USDA PLANTS symbols if they exist.
fill.usda.symbols <- function(taxa, symbol=NA){
  x  <-  data.frame(taxa=taxa, symbol=symbol)
  x <- x |> left_join(usdaplants[,c('taxon','sym')], by=c('taxa'='taxon'), multiple = 'first')
  x <- x |> mutate(symbol = ifelse(is.na(symbol), sym, symbol))
  return(x$symbol)}

#This function fills in missing plant names from USDA symbols if they exist.
fill.taxon.from.symbols <- function(symbol, taxa=NA){
  x  <-  data.frame(symbol=symbol, taxa=taxa)
  x <- x |> left_join(usdaplants[,c('taxon','sym')], by=c('symbol'='sym'), multiple = 'first')
  x <- x |> mutate(taxon = ifelse(is.na(taxa), taxon, taxa))
  return(x$taxon)}
