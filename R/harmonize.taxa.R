#upgrade taxonomy ----
#This function synonymizes taxa with BONAP or Kew's Plants of the World Online circa 2022.
harmonize.taxa <- function(taxa, fix=FALSE){
  x  <-  data.frame(taxa=taxa)
  if(fix){
  fixtaxa <- data.frame(america=c('Cornus sericea', 'Equisetum prealtum', 'Athyrium angustum', 'Cypripedium parviflorum' , 'Osmunda spectabilis'), auctnon=c('Cornus alba','Equisetum hyemale', 'Athyrium filix-femina', 'Cypripedium calceolus', 'Osmunda regalis'))
  x <- x |> left_join(fixtaxa, by=c('taxa'='auctnon'), multiple = 'first')
  x <- x |> mutate(taxa = ifelse(is.na(america), taxa, america))
  }
 x <- x |> left_join(syns[,c('acc','syn','ac.binomial')], by=c('taxa'='syn'), multiple = 'first')
  x <- x |> mutate(ac.binomial = ifelse(is.na(ac.binomial), taxa, ac.binomial))

  return(x$ac.binomial)}

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
