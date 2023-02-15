#upgrade taxonomy ----
#This function synonymizes taxa with BONAP or Kew's Plants of the World Online circa 2022.
harmonize.taxa <- function(taxa){
  x  <-  data.frame(taxa=taxa)
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
