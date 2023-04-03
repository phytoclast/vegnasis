#This function establishes a hexagonal grid to place plants
make_hex_stand <- function(hects=1, minsize=1){
  #scale larger
  x = (1:(100*hects))
  y = (1:(116*hects))
  set.seed(42)
  m <- merge(x,y) |> as.data.frame()
  m <- m |> mutate(x = ifelse(floor(y/2)==y/2, x-0.5,x), y = round(y*(3^0.5)/2,2), wt=1)
  pref=3
  f= pref^1
  mx1 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f^2)  |> subset(x <=100*hects & y <= 100*hects)
  f = pref^2
  mx2 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f^2)  |> subset(x <=100*hects & y <= 100*hects)
  f = pref^3
  mx3 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f^2)  |> subset(x <=100*hects & y <= 100*hects)
  mm <- rbind(m,mx1,mx2,mx3) |> group_by(x,y) |> summarise(wt=max(wt)) |> as.data.frame()
  mm <- mm |> mutate(x = x*minsize, y = y*minsize)
  colnames(mm) <- c('xp','yp','wt')
  rownames(mm) <- 1:nrow(mm) |> as.numeric()
  mm$stumpid <- rownames(mm)
  return(mm)
}

#This mixes a background color to an objects color to help object fade into distance and give illusion of depth
colormixer <- function(colorname, mixcolor, p){
  ccc <- col2rgb(colorname)
  ccc <- data.frame(r = ccc[1,],   g = ccc[2,],   b = ccc[3,])
  mmm <- col2rgb(mixcolor)
  new <- ccc |> mutate(r = r*(1-p)+mmm[1,1]*p,
                       g = g*(1-p)+mmm[2,1]*p,
                       b = b*(1-p)+mmm[3,1]*p)
  new <- rgb(new$r/255,new$g/255,new$b/255)
  return(new)
}

#These functions take shapes and assemble them according to plant attributes by stratum.
make_tree <- function(ht.max, ht.min,crwd,dbh, crshape, stshape){
  crown <- subset(shapes, shape %in% crshape) |> mutate(x=x*crwd, z=z*(ht.max-ht.min)+ht.min, obj='crown')
  base <- subset(shapes, shape %in% stshape) |> mutate(x=x*dbh/100*1.1, z=z*(ht.min), obj='stem')
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}
make_shrub <- function(ht.max, ht.min,crwd, crshape, stshape){
  crown <- subset(shapes, shape %in% crshape)  |> mutate(x=x*crwd, z=z*(ht.max-ht.min)+ht.min, obj='crown')
  base <- subset(shapes, shape %in% stshape) |> mutate(x=x*crwd*0.8, z=z*(ht.min), obj='stem')
  shrub = rbind(crown, base)
  shrub$ptord <- rownames(shrub) |> as.numeric()
  return(shrub)}
make_herb <- function(ht.max,crwd, crshape){
  herb <- subset(shapes, shape %in% crshape)  |> mutate(x=x*crwd, z=z*ht.max, obj='herb')
  herb$ptord <- rownames(herb) |> as.numeric()
  return(herb)}

make_plant<- function(fun, ht.max, ht.min,crwd,dbh, crshape, stshape){
  if(fun %in% 'T'){
    plant <- make_tree(ht.max, ht.min,crwd,dbh, crshape, stshape)}else
      if(fun %in% 'S'){
        plant <- make_shrub(ht.max, ht.min, crwd, crshape, stshape)}else
        {
          plant <- make_herb(ht.max,crwd, crshape)}
}
