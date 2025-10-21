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
#' Color mixer
#'
#' @param colorname Name or hex code for main color of object (can include alpha for transparency).
#' @param mixcolor Color used for fading into (maintaining the same alpha as main color).
#' @param p proportion of fading into the mix color.
#'
#' @returns Faded color.
#' @export
#'
#' @examples crowncolor='darkgreen'; fadecolor = "#D9F2FF"
#' @examples colormixer(crowncolor, fadecolor, 0.5)
colormixer <- function(colorname, mixcolor, p){
  ccc <- col2rgb(colorname, alpha = TRUE)
  ccc <- data.frame(r = ccc[1,],   g = ccc[2,],   b = ccc[3,], a = ccc[4,])
  mmm <- col2rgb(mixcolor)
  new <- ccc |> mutate(r = r*(1-p)+mmm[1,1]*p,
                       g = g*(1-p)+mmm[2,1]*p,
                       b = b*(1-p)+mmm[3,1]*p,
                       a = a)
  new <- rgb(new$r, new$g, new$b, alpha = new$a,  maxColorValue = 255)
  return(new)
}


#These functions take shapes and assemble them according to plant attributes by stratum.
make_tree <- function(ht.max, ht.min, crwd, dbh, crshape, stshape){
  shapes <- vegnasis::shapes
  if(crshape %in% 'branch.conifer'){
    tree <- tree.001(ht.max,
                     ht.min,
                     crwd,
                     dbh)
  }else if(crshape %in% 'branch.hardwood'){
    tree <- tree.002(ht.max,
                     ht.min,
                     crwd,
                     dbh)
  }else{

    crown <- subset(shapes, shape %in% crshape) |> mutate(x=x*crwd, z=z*(ht.max-ht.min)+ht.min, obj='crown')
    base <- subset(shapes, shape %in% stshape) |> mutate(x=x*dbh/100*1.1, z=z*(2*ht.min+ht.max)/3, obj='stem')
    tree = rbind(crown, base)
    tree$ptord <- rownames(tree) |> as.numeric()}

  return(tree)}

make_shrub <- function(ht.max, ht.min,crwd, crshape, stshape){
  shapes <- vegnasis::shapes
  crown <- subset(shapes, shape %in% crshape)  |> mutate(x=x*crwd, z=z*(ht.max-ht.min)+ht.min, obj='crown')
  base <- subset(shapes, shape %in% stshape) |> mutate(x=x*crwd*0.8, z=z*(ht.min), obj='stem')
  shrub = rbind(crown, base)
  shrub$ptord <- rownames(shrub) |> as.numeric()
  return(shrub)}
make_herb <- function(ht.max,crwd, crshape){
  shapes <- vegnasis::shapes
  herb <- subset(shapes, shape %in% crshape)  |> mutate(x=x*crwd, z=z*ht.max, obj='herb')
  herb$ptord <- rownames(herb) |> as.numeric()
  return(herb)}

make_plant<- function(fun, ht.max, ht.min,crwd,dbh, crshape, stshape){
  if(fun %in% 'T'){
    plant <- make_tree(ht.max, ht.min,crwd,dbh, crshape, stshape)
  }else
    if(fun %in% 'S'){
      plant <- make_shrub(ht.max, ht.min, crwd, crshape, stshape)}else
      {
        plant <- make_herb(ht.max,crwd, crshape)
      }
  plant <- plant |> mutate(fill=NULL,color=NULL)
  return(plant)
}



make_plant.overhead<- function(crwd, dbh, crshape, stshape){
  crshape <- case_when(crshape %in% c('conifer','conifer1','conifer2','conifer3','palm','boreal',
                                      "araucaria","cactus", "fanpalm","featherpalm","fir_old",
                                      "fir","hemlock","longleaf_sap","palmetto","spruce",
                                      "subalpine","wcedar","wpine", "ypine","yucca") ~ 'conifer',
                       TRUE ~ 'hardwood')
  crown <- subset(overheadshapes, shape %in% crshape) |> mutate(x=x*crwd, y=y*crwd, obj='crown')
  base <- subset(overheadshapes, shape %in% 'circle') |> mutate(x=x*dbh/100*1.1, y=y*dbh/100*1.1, obj='stem')
  plant = rbind(crown, base)
  plant$ptord <- rownames(plant) |> as.numeric()

  return(plant)
}


####
tree.001 <- function(ht.max,
                     ht.min,
                     crwd,
                     dbh){
  # ht.max=15
  # ht.min=5
  # crwd=5
  # dbh=40
  bu=1
  bl=0.1
  opposite=T
  oppfactor = ifelse(opposite,1,2)
  n = pmax(floor(10*(ht.max-ht.min)*(bu-bl)/10*oppfactor/crwd*5),1)
  crshape = c('pyramid')

  bf <- ifelse(opposite, 5/n,10/n)
  shapes <- makeCrownShape(ht.max=ht.max,ht.min=ht.min, crwd=crwd, dbh=dbh/100, n=n, bu=bu, bl=bl, crshape=crshape,opposite = opposite)

  shapes <- subset(shapes, !(a > 175 | a < -175 | a == 0)  & l> 0.15 & by < ht.max)
  stem <-  makeStem(ht.max,dbh/100,0.05,15)
  cstem <- stem
  for(i in 1:nrow(shapes)){#i=1
    branch <- makeStem(shapes$l[i], shapes$d[i]*0.5,0.01,10)
    bpos <- max(branch$y)
    twigA <- branch |> mutate(x=x*0.4*bf,y=y*0.4*bf)
    twigB <- branch |> mutate(x=x*0.3*bf,y=y*0.3*bf)
    twigC <- branch |> mutate(x=x*0.2*bf,y=y*0.2*bf)
    branchx1 <- branch |> mutate(x=x*0.3*bf,y=y*0.3*bf)
    branch <- skewStem(branch, amp=ifelse(shapes$a[i] >= 0,-0.07*(shapes$s[i]*-1+1),0.07*(shapes$s[i]*-1+1)),
                       phase=0, waves=1)
    # branchA <- attachBranch(branch,  twigB, ifelse(shapes$a[i] >= 0,40,-40), bpos*0.15)#lower
    # branchA <- attachBranch(branchA, twigC, ifelse(shapes$a[i] >= 0,40,-40), bpos*0.3)#lower
    branchA <- attachBranch(branch, twigB, ifelse(shapes$a[i] >= 0,35,-35), bpos*0.4)#lower
    # branchA <- attachBranch(branchA, twigC, ifelse(shapes$a[i] >= 0,35,-35), bpos*0.5)#lower
    # branchA <- attachBranch(branchA, twigB, ifelse(shapes$a[i] >= 0,30,-30), bpos*0.6)#lower
    # branchA <- attachBranch(branchA, twigC, ifelse(shapes$a[i] >= 0,30,-30), bpos*0.7)#lower
    branchA <- attachBranch(branchA, twigC, ifelse(shapes$a[i] >= 0,-30,30), bpos*0.7)#upper
    # branchA <- attachBranch(branchA, twigB, ifelse(shapes$a[i] >= 0,-30,30), bpos*0.5)#upper
    #basal branch twigs to attach crown
    branchB <- attachBranch(branchA, branchx1, ifelse(shapes$a[i] >= 0,-90,90), bpos*0.05)
    branchB <- attachBranch(branchB, branchx1, ifelse(shapes$a[i] >= 0,30,-30), bpos*0.05)

    stem <- attachBranch(stem, branchA, shapes$a[i], shapes$by[i])#branches to show bare
    cstem <- attachBranch(cstem, branchB, shapes$a[i], shapes$by[i])#branches to attach crown
  }
  crown <- cstem |> subset(grepl('tip',type))

  crown <- crown |> mutate(z=y, shape = 'borealcrown',  fill='green', color='darkgreen', obj='crown', ptord=i) |> select(c("x","z","shape","fill","color","obj","ptord"))
  stem <- stem |> mutate(z=y, shape = 'borealstem',  fill='orange', color='brown', obj='stem', ptord=i) |> select(c("x","z","shape","fill","color","obj","ptord"))
  newtree = rbind(stem,crown)
  return(newtree)
}
tree.002 <- function(ht.max,
                     ht.min,
                     crwd,
                     dbh){
  # ht.max=15
  # ht.min=5
  # crwd=5
  # dbh=40
  bu=0.5#as crown widens, make top and bottom lower
  bl=-0.3#make sure tree is tall enough to support negative
  opposite=F
  oppfactor = ifelse(opposite,1,2)
  n = pmax(floor(7*(ht.max-ht.min)*(bu-bl)/10*oppfactor/crwd*5),1)
  crshape = c('dome')
  ca <- pmin(1,(crwd*(ht.max-ht.min)*(bu-bl)/80))

  shapes <- makeCrownShape(ht.max=ht.max,ht.min=ht.min, crwd=crwd, dbh=dbh/100, n=n, bu=bu, bl=bl, crshape=crshape,opposite = opposite)
  shapes <- subset(shapes, !(a > 175 | a < -175 | a == 0)  & l> 0.15 & by < ht.max)

  bf <- ifelse(opposite, 5/n,10/n)#twig size based on number of branches
  af <- pmin(1,mean(abs(shapes$a))/30)#twig size based on angles

  stem <-  makeStem(ht.max,dbh/100,0.02,15)
  stem <- skewStem(stem, amp = 0.1, phase = 0, waves = 1)

  branch <- makeStem(crwd*0.1*ca*af, dbh/100*0.1,0.01,10)#small branch near top
  stem <- attachBranch(stem, branch, -30*af, ht.max*0.93)
  cstem <- stem
  for(i in 1:nrow(shapes)){#i=1
    branch <- makeStem(shapes$l[i], shapes$d[i]*0.5,0.01,10)
    bpos <- max(branch$y)
    twigA <- branch |> mutate(x=x*0.4*bf,y=y*0.4*bf*af)
    twigB <- branch |> mutate(x=x*0.2*bf,y=y*0.2*bf*af)
    twigB <- skewStem(twigB, amp=ifelse(shapes$a[i] >= 0,
                                        -0.05*(af*bf*-1+1),
                                        0.05*(af*bf*-1+1)),
                      phase=0.5, waves=1.5)
    twigC <- branch |> mutate(x=x*0.1*bf,y=y*0.1*bf*af)
    twigC <- skewStem(twigC, amp=ifelse(shapes$a[i] >= 0,
                                        -0.03*(af*bf*-1+1),
                                        0.03*(af*bf*-1+1)),
                      phase=0.5, waves=1.5)
    branchx1 <- branch |> mutate(x=x*0.3*bf,y=y*0.3*bf*af)
    branch <- skewStem(branch, amp=ifelse(shapes$a[i] >= 0,-0.07*(shapes$s[i]*-1+1),0.07*(shapes$s[i]*-1+1)), phase=0.5, waves=2)
    branchA <- attachBranch(branch, twigB, ifelse(shapes$a[i] >= 0,30,-30), bpos*0.3)#lower
    branchA <- attachBranch(branchA, twigC, ifelse(shapes$a[i] >= 0,-30,30), bpos*0.7)#upper
    #basal branch twigs to attach crown
    branchB <- attachBranch(branchA, branchx1, ifelse(shapes$a[i] >= 0,-90,90), bpos*0.05)
    branchB <- attachBranch(branchB, branchx1, ifelse(shapes$a[i] >= 0,30,-30), bpos*0.05)
    stem <- attachBranch(stem, branchA, shapes$a[i], shapes$by[i])#branches to show bare
    cstem <- attachBranch(cstem, branchB, shapes$a[i], shapes$by[i])#branches to attach crown
  }
  crown <- cstem |> subset(grepl('tip',type))
  crown <- vegnasis::cavhull(x=crown$x,y=crown$y, concavity = 0)


  crown <- crown |> mutate(z=y, shape = 'hardwoodcrown',  fill='green', color='darkgreen', obj='crown', ptord=i) |> select(c("x","z","shape","fill","color","obj","ptord"))
  stem <- stem |> mutate(z=y, shape = 'hardwoodcrown',  fill='orange', color='brown', obj='stem', ptord=i) |> select(c("x","z","shape","fill","color","obj","ptord"))
  newtree = rbind(stem,crown)
  return(newtree)
}

