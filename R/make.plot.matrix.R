# remotes::install_github("phytoclast/vegnasis", dependencies = FALSE)
# library(vegnasis)
#
# data('nasis.veg')
#
# veg <- clean.veg(nasis.veg)
# x = veg
# This function takes a standardized vegetation plot dataframe and converts it to a community matrix that can be converted to a distance matrix. Parameters include options for transformations and whether to use absolute or relative cover. Choices for transformation include: "log" for log base 10, "sqrt" for square root, and nothing for no transformation.

make.plot.matrix <- function(x, tr = NA, rc = TRUE, nr=FALSE, label='plot', taxon='taxon'){
  x <- as.data.frame(x)#Tibbles lead to unexpected failures
  if(!label %in% 'plot'){
    plots <- data.frame(plot =   unique(x$plot)) |> mutate(rnum = 1:length(unique(x$plot)))
    x <- x |> left_join(plots)
    x$plot <- paste0(x$rnum,'.', x[,label])}
  x$taxon = x[,taxon]
  x <- x |> group_by(plot, taxon) |> summarise(cover = cover.agg(cover))
  if(rc){x <- x |> group_by(plot) |> mutate(cover= cover/(sum(cover)*100+0.0000001))}
  if(nr){x <- x |> group_by(taxon) |> mutate(cover= cover/(max(cover)*100+0.0000001))}
  df <- data.frame(r=x$plot, c=x$taxon, v=x$cover) |> subset(nchar(r) >= 1 & nchar(c) >= 1)
  if(tr %in% 'log'){
    df$v <- (log10(df$v+0.1)+1)/(log10(100+0.1)+1)*100
  }else if(tr %in% 'sqrt'){
    df$v <- df$v^0.5*10}
  df <- df |>  mutate(r = as.factor(stringr::str_replace_all(r, '\\s+', '.')), c = as.factor(stringr::str_replace_all(c, '\\s+', '.')))
  kc <- levels(df$c)
  kr <- levels(df$r)
  nc <- length(kc)
  nr <- length(kr)
  m <- matrix(0, nrow=nr, ncol=nc)
  for (j in 1:nc){
    for(i in 1:nr){#j=6; i=7
      v = df[df$r %in% kr[i] & df$c %in% kc[j],]$v
      m[i,j] <- ifelse(!length(v)>0, 0,v)
    }}

  m <- as.data.frame(m)
  colnames(m) <- kc; rownames(m) <- kr

  return(m)
}
# d = vegan::vegdist(m, method='bray')
# t <- cluster::agnes(d, method = 'ward')|> as.hclust()
# t <- optpart::flexbeta(d, beta = -0.2)|> as.hclust()
# plot(t)
# ape::plot.phylo(ape::as.phylo(t))

