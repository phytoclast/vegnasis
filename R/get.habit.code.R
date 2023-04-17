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
get.habit.name <- function(code,type='name'){
  x  <-  as.data.frame(cbind(code=code)) |>
    left_join(gho, by = c('code'='Revised.Symbol'), multiple = 'first')
  if(type %in% 'stem'){return(x$First)}else if(type %in% 'ESIS'){return(x$ESIS.Group)}else{return(x$Habitname)}
}


#Lookup default maximum plant height for when no empirical heights were recorded ----
get.ht.max <- function(taxa){
  x  <-  as.data.frame(cbind(taxa=taxa)) |> mutate(ht0 = NA_real_, genus = str_split_fixed(taxa , '[[:blank:]]',3)[,1])
  #first try straight join ----
  x <- x |> left_join(taxon.habits[,c('Scientific.Name','ht.max')], by = c('taxa'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(ht0 = ifelse(is.na(ht0)| ht0 %in% NA_real_, ht.max, as.character(ht0)))
  x <- x[,1:3]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','syn')], by=c('taxa'='syn'), multiple = 'first') |> left_join(taxon.habits[,c('Scientific.Name','ht.max')], by = c('acc'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(ht0 = ifelse(is.na(ht0)| ht0 %in% "", ht.max, as.character(ht0)))
  x <- x[,1:3]
  #finally try genus only ----
  x <- x |> left_join(genus.habits, by = c('genus'='genus'), multiple = 'first')
  x <- x |> mutate(ht0 = ifelse(is.na(ht0)| ht0 %in% "", ht.max, as.character(ht0)))
  x <- x[,1:2]
  return(as.numeric(x$ht0)) }



#Lookup the NASIS life form "type" according to default growth habit of each taxon. Include column of existing types if existing types are to be preserved while filling in only missing values.

fill.type <- function(taxa, type=NA){
  x  <-  data.frame(taxa=taxa, type = type) |> mutate(GH0 = '', genus = str_split_fixed(taxa , '[[:blank:]]',3)[,1])
  #first try straight join ----
  x <- x |> left_join(taxon.habits[,c('Scientific.Name','GH')], by = c('taxa'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:4]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','syn')], by=c('taxa'='syn'), multiple = 'first') |> left_join(taxon.habits[,c('Scientific.Name','GH')], by = c('acc'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:4]
  #finally try genus only ----
  x <- x |> left_join(genus.habits, by = c('genus'='genus'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:3]
  x <- x |> mutate(nasis = case_when(
    grepl('^T', GH0) ~ 'tree',
    grepl('^S', GH0)| grepl('^L', GH0) | grepl('^E', GH0) ~ 'shrub/vine',
    grepl('^H.G', GH0)~ 'grass/grasslike',
    grepl('^H', GH0)~ 'forb',
    grepl('^N.B', GH0)~ 'moss',
    grepl('^N.L', GH0)~ 'lichen',
    grepl('^N', GH0)~ 'microbiotic crust',
    TRUE ~ NA
  ))
  nasis <- ifelse(is.na(x$type), x$nasis, x$type)
  return(nasis) }

fill.type.ESIS <- function(taxa, type=NA){
  x  <-  data.frame(taxa=taxa, type = type) |> mutate(GH0 = '', genus = str_split_fixed(taxa , '[[:blank:]]',3)[,1])
  #first try straight join ----
  x <- x |> left_join(taxon.habits[,c('Scientific.Name','GH')], by = c('taxa'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:4]
  #then try synonym join ----
  x <- x |> left_join(syns[,c('acc','syn')], by=c('taxa'='syn'), multiple = 'first') |> left_join(taxon.habits[,c('Scientific.Name','GH')], by = c('acc'='Scientific.Name'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:4]
  #finally try genus only ----
  x <- x |> left_join(genus.habits, by = c('genus'='genus'), multiple = 'first')
  x <- x |> mutate(GH0 = ifelse(is.na(GH0)| GH0 %in% "", GH, as.character(GH0)))
  x <- x[,1:3]
  x <- x |> mutate(esis = case_when(
    grepl('^T.F', GH0)|grepl('^S.F', GH0) ~ 'Tree Fern',
    grepl('^T', GH0) ~ 'Tree',
    grepl('^S', GH0) ~ 'Shrub/Subshrub',
    grepl('^L', GH0) | grepl('^E', GH0) ~ 'Vine/Liana',
    grepl('^H.G', GH0)~ 'Grass/grass-like',
    grepl('^H.FE', GH0)~ 'Fern/fern ally',
    grepl('^H', GH0)~ 'Forb/Herb',
    grepl('^N.B', GH0)~ 'Nonvascular',
    grepl('^N.L', GH0)~ 'Biological Crusts',
    grepl('^N', GH0)~ 'Biological Crusts',
    TRUE ~ NA
  ))
  nasis <- ifelse(is.na(x$type), x$esis, x$type)
  return(nasis) }


#Lookup the NASIS life form "type" according to default growth habit of each taxon. Include column of existing types if existing types are to be preserved while filling in only missing values. This version is intended for cleaned data frames.

fill.type.df <- function(df){
  df$type <- fill.type(df$taxon, df$type)
  return(df) }

fill.type.ESIS.df <- function(df){
  df$type <- fill.type.ESIS(df$taxon, df$type)
  return(df) }



