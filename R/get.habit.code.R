#Lookup the growth habit code from a vector of taxon names ----
get.habit.code <- function(taxa){
  x  <-  as.data.frame(cbind(taxa=taxa)) |> mutate(GH0 = '', genus = str_split_fixed(taxa , '[[:blank:]]',3)[,1])
  #first try straight join ----
  x <- x |> left_join(taxon.habits[,c('Scientific.Name','GH')], by = c('taxa'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:3]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','syn')], by=c('taxa'='syn'), multiple = 'first') |> left_join(taxon.habits[,c('Scientific.Name','GH')], by = c('acc'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:3]
  #finally try genus only ----
  x <- x |> left_join(genus.habits, by = c('genus'='genus'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:2]
  return(x$GH0) }






#Lookup the growth habit descriptive name or ESIS  from a vector of growth habit codes ----
get.habit <- function(code,type='name'){
  x  <-  as.data.frame(cbind(code=code)) |>
    left_join(gho, by = c('code'='Revised.Symbol'), multiple = 'first')
  if(type %in% 'stem'){return(x$First)}else if(type %in% 'ESIS'){return(x$ESIS.Group)}else{return(x$Habitname)}
}
