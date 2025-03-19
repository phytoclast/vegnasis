#' Make Stem
#'
#' @param lth Length of stem.
#' @param wth Width of stem at base.
#' @param tip Width of stem at tip.
#' @param inc Number of vertices increments to include (how many dots per side).
#'
#' @returns Simple stem data frame with xy coordinates for plotting.
#' @export
#'
#' @examples
makeStem <- function(lth, wth, tip=0.01, inc=10){


  s <- (0:inc)/inc
  i <- c(1:(inc+1),(1:(inc+1))+inc+1)
  y <- s*lth; y <- c(y,y[(inc+1):1])
  x <- tip/2*s+wth/2*(s*-1+1); x <- c(-1*x,x[(inc+1):1])
  side <- ifelse(x >= 0, 'R','L')
  df <- data.frame(x = x,
                   y = y,
                   i = i,
                   type = 'mid',
                   side=side,
                   center=0,
                   width=abs(x*2)
  )
  df[c(1,nrow(df)),]$type <- 'base'
  df[c(nrow(df)/2,nrow(df)/2+1),]$type <- 'tip'
  return(df)
}


#add options to add branch constraint by height and crown width rather than just angle

#' Attach branch to stem
#'
#' @param stem Name of the primary stem object.
#' @param branch Name of secondary stem object to be used as a branch.
#' @param angle Angle of branch from the vertical in degrees.
#' @param bht Height of branch attachment point on stem.
#' @param tht Optional total height of tree instead of specifying angle.
#' @param tx
#'
#' @returns Stem object with attached branch.
#' @export
#'
#' @examples
attachBranch <- function(stem, branch, angle=90, bht, tht=NA, tx=NA){
  #establish permanent columns to end up with
  original <- colnames(stem)

  #get information about stem length
  stemax <- max(subset(stem, type %in% c('base','mid','tip'))$y)
  stemin <- min(subset(stem, type %in% c('base','mid','tip'))$y)

  #ensure that branch never rises higher than stem
  bht <- ifelse(bht > stemax,stemax,bht)

  #ensure that branch is no thicker than stem
  bhtlower <- max(stem[bht >= stem$y,]$y)
  bhtupper <- min(stem[bht <= stem$y,]$y)
  swd <- (mean(subset(stem, y == bhtlower)$width)*1/(abs(bht-bhtlower)+0.01)+
            mean(subset(stem, y == bhtupper)$width)*1/(abs(bht-bhtupper)+0.01))/(1/(abs(bht-bhtlower)+0.01)+1/(abs(bht-bhtupper)+0.01))
  bbase <- subset(branch, type %in% 'base')
  bwd <- max(bbase$x)-min(bbase$x)
  # if(bwd > swd) {branch <- branch |> mutate(x = x*swd/bwd)}

  #convert angle to radians
  angle = angle/360*2*pi

  #optional establish branch angle based on branch distance from stem
  if(!is.na(tht) & !is.na(tx)){
    l = ((tht-bht)^2+(tx)^2)^0.5
    bmaxlen <- max((branch$x^2+branch$y^2)^0.5)
    #resize branch to fit specified height and width
    branch <- branch |> mutate(x=x*l/bmaxlen, y=y*l/bmaxlen)
    angle <- asin(tx/l)
  }



  #set which side of stem branch will be fitted
  xside <- ifelse(angle>0,'R','L')
  xsine <- ifelse(angle>0,1,-1)

  #rotate branch to correct angle
  branch <- branch |> mutate(h=(x^2+y^2)^0.5,a = acos(y/h),a=ifelse(x < 0,-1*a,a),
                             x = h*sin(a+angle), y = h*cos(a+angle))

  #lift branch to correct height
  branch <- branch |> mutate(y = y+bht)

  #recheck to see if branch thickness pushes branch too high
  bdif <- stemax - max(branch[branch$type %in% 'base',]$y)
  if(bdif < 0){branch <- branch |> mutate(y = y+bdif)}

  #determine which stem vertices straddle the branch
  xd <- subset(stem, side %in% xside & type %in% c('tip','mid','base','bbase'))
  ylower <- max(subset(xd, y < min(branch[branch$type %in% 'base',]$y))$y)
  yupper <- min(subset(xd, y >= max(branch[branch$type %in% 'base',]$y))$y)
  xupper <- subset(xd, y == yupper)$x
  xlower <- subset(xd, y == ylower)$x

  #shift branch to conform with twisted stem center position
  cshift <- mean(subset(xd, y == yupper | y == ylower)$center)
  branch <- branch |> mutate(x = x+cshift)

  #identify which branch vertices are inside stem
  branch <- branch |> mutate(inside = (x-((y-ylower)/(yupper-ylower)*(xupper-xlower)+xlower))*xsine)

  #identify vertices which straddle inside and outside of stem to find points of intersection
  internal <- branch |> mutate(near = inside^2, isinside = ifelse(inside < 0 | type %in% 'base', 'no','yes')) |> group_by(isinside, side) |> mutate(minnear = min(near)) |> ungroup() |> subset(near == minnear)

  #approximate location of branch stem intersection to establish new branch base
  newbase <- internal |> group_by(side) |> mutate(amt = 1/((inside - 0)/(max(inside)-min(inside)+0.0001))^2) |>
    summarise(x= sum(amt*x)/sum(amt), y= sum(amt*y)/sum(amt), i= mean(i), type='base', center=0, side=xside, width=bwd)


  #determine if stem if branch too close to top, less than branch width
  if(stemax - bht < bwd*2){
    #identify where to insert new numbering sequence to maintain correct vertex order
    xdi <- mean(subset(stem, type %in% 'tip')$i)
    #remove tip of stem
    steminternal <- stem |> subset(!type %in% 'tip' & !(y > (stemax - 0.05*(stemax-stemin)) & type %in% 'mid'))
    #assemble branch with new base, omitting internal vertices
    branchinternal <- branch |> subset(select=original) |> rbind(newbase[,original]) |> mutate(i = xdi + i/10000, type=paste0('b',type), inside = NULL, h=NULL,a=NULL)  |> arrange(i)
  }else{
    #remove stem vertices that may be covered by new branch
    steminternal <- stem |> subset(!(x >= min(internal$x) & x <= max(internal$x) & y >= min(internal$y) & y <= max(internal$y))) |> subset(select=original)
    #identify stem vertices near branch base
    ylower2 <- max(subset(xd, y < min(newbase$y))$y)
    yupper2 <- min(subset(xd, y > max(newbase$y))$y)
    #identify where to insert new numbering sequence to maintain correct vertex order
    xdi <- mean(subset(xd, y %in% c(yupper2, ylower2))$i)
    #assemble branch with new base, omitting internal vertices
    branchinternal <- branch |> subset(inside >= 0) |> subset(select=original) |> rbind(newbase[,original]) |> mutate(i = xdi + i/10000, type=paste0('b',type), inside = NULL, h=NULL,a=NULL)  |> arrange(i)

  }


  #append branch to stem with correct vertex order
  stemnew <- rbind(branchinternal,steminternal) |> arrange(i)

  #renumber vertices
  stemnew <- mutate(stemnew, i=(1:nrow(stemnew)))

  return(stemnew)}


#' Skew Stem
#'
#' @param stem Name of stem object.
#' @param amp Amplitude of bend.
#' @param phase Shift in the position of the bend in the stem.
#' @param waves Number of bends in the stem.
#'
#' @returns Stem object with bends introduced to make stem look twisted.
#' @export
#'
#' @examples
skewStem <- function(stem, amp=0.2, phase=0, waves=1){
  maxstem <- max(stem$y)
  minstem <- min(stem$y)
  lth <- maxstem - minstem
  stem <-  stem |> mutate(x = x + amp*cos((y/lth+phase)*2*pi*waves),
                          center = center + amp*cos((y/lth+phase)*2*pi*waves))
  return(stem)
}

#' Make Tree Crown Outline
#'
#' @param ht.max Height of the tree.
#' @param ht.min Height of the lowest branches.
#' @param crwd Crown width.
#' @param dbh Stem base diameter.
#' @param tip Thickness of stem tip.
#' @param crshape Basic crown shape (pyramid, dome, round, or column).
#' @param n Number of branches.
#' @param bu Relative position (0-1) of highest branch attachment to tree relative to the top and bottom of the crown.
#' @param bl Relative position (0-1) of lowest branch attachment to tree relative to the top and bottom of the crown.
#' @param opposite If true the all branches will be placed opposite each other on stem. If false, the branches will alternate the right and left side of stem.
#'
#' @returns A data frame with information for attaching branches to a stem.
#' @export
#'
#' @examples #fractal tree with bend ---
#' library(ggplot2)
#' n=3
#' lth=4
#' wth = 0.5
#' sc = 0.6
#' stem <-  makeStem(lth,wth,0.1*wth,20)
#' stem <-  stem |> mutate(x = x + -0.2*cos(y/lth*2*pi), center = center + -0.2*cos(y/lth*2*pi))
#' stem <-  skewStem(stem,-0.2,.5,3)
#' branch <-  stem |> mutate(x=x*sc,y=y*sc,center=center*sc)
#' branch2 <-  stem |> mutate(x=x*sc*0.7,y=y*sc*0.7,center=center*sc*0.7)
#' branch3 <-  stem |> mutate(x=x*sc*0.3,y=y*sc*0.3,center=center*sc*0.3)
#' for(i in 1:n){
#'   tree <- attachBranch(stem, branch, -50, 2)
#'   branch <- branch2
#'   tree <- attachBranch(tree, branch, 50, 3)
#'   branch <- branch3
#'   #stem=tree
#'   tree <- attachBranch(tree, branch, -5, 4)
#'   branch <- tree |> mutate(x=x*sc,y=y*sc,center=center*sc)
#'   branch2 <- tree |> mutate(x=x*sc*.7,y=y*sc*.7,center=center*sc*.7)
#'   branch3 <- tree |> mutate(x=x*sc*.3,y=y*sc*.3,center=center*sc*.3)
#' }
#'
#' crown <- tree |> subset(grepl('tip',type) & !type %in% 'tip')
#' ggplot()+
#'   geom_polygon(data=tree, aes(x=x, y=y), color='brown',fill='#99500090')+
#'   # geom_point(data=stem, aes(x=x, y=y), color='red')+
#'   geom_polygon(data=crown, aes(x=x, y=y), color='green',fill='#00990090')+
#'   # geom_point(data=tree, aes(x=x, y=y), color='green')+
#'   coord_fixed()
#'
#'   #------conifer
#'
#' crshape = c('pyramid','dome','round','column')
#'
#' crshape = c('pyramid','column')
#' shapes <- makeCrownShape(ht.max=10, ht.min=3, crwd=3, dbh=0.5, n=7, bu=1, bl=0.2, crshape=crshape,
#'                          opposite = T)
#' shapes <- subset(shapes, !(a > 175 | a < -175 | a == 0)  & l> 0.3)
#' stem <-  makeStem(10,0.5,0.01,10)
#' for(i in 1:nrow(shapes)){#i=1
#'   branch <- makeStem(shapes$l[i], shapes$d[i]*.5,0.01,10)
#'   bpos <- max(branch$y)
#'   branch2 <- branch |> mutate(x=x*0.3,y=y*0.3)
#'   branch3 <- branch |> mutate(x=x*0.2,y=y*0.2)
#'
#'   branch <- skewStem(branch, amp=ifelse(shapes$a[i] >= 0,-0.07*(shapes$s[i]*-1+1),0.07*(shapes$s[i]*-1+1)),
#'                      phase=0, waves=1)
#'   branchA <- attachBranch(branch, branch2, ifelse(shapes$a[i] >= 0,30,-30), bpos*0.3)
#'   branchA <- attachBranch(branchA, branch3, ifelse(shapes$a[i] >= 0,-30,30), bpos*0.7)
#'
#'
#'   stem <- attachBranch(stem, branchA, shapes$a[i], shapes$by[i])
#' }
#' crown <- stem |> subset(grepl('tip',type) | type %in% 'bbase')
#'
#'
#' ggplot()+
#'   geom_polygon(data=stem, aes(x=x, y=y), color='brown',fill='#99500090')+
#'   # geom_point(data=stem, aes(x=x, y=y), color='red')+
#'   geom_polygon(data=crown, aes(x=x, y=y), color='green',fill='#00990090')+
#'   # geom_point(data=tree, aes(x=x, y=y), color='green')+
#'   coord_fixed()
#'
#'   #------cottonwood
#'
#'   crshape = c('pyramid','dome','round','column')
#'
#'   crshape = c('dome')
#'   shapes <- makeCrownShape(ht.max=10, ht.min=5, crwd=6, dbh=0.5, n=3, bu=0.5, bl=0, crshape=crshape,
#'   opposite = F)
#'   shapes <- subset(shapes, !(a > 175 | a < -175 | a==0)  & l> 0.3)
#'   stem <-  makeStem(10,0.5,0.01,25)
#'   stem <- skewStem(stem, amp=-0,
#'                    phase=0, waves=0.5)
#'
#'   for(i in 1:nrow(shapes)){#i=1
#'     branch <- makeStem(shapes$l[i], shapes$d[i]*.5,0.01,15)
#'     bpos <- max(branch$y)
#'     branch2 <- branch |> mutate(x=x*0.3,y=y*0.3)
#'     branch3 <- branch |> mutate(x=x*0.2,y=y*0.2)
#'
#'     branch <- skewStem(branch, amp=ifelse(shapes$a[i] >= 0,-0.2,0.2),
#'                        phase=0, waves=1)
#'     branchA <- attachBranch(branch, branch2, ifelse(shapes$a[i] >= 0,30,-30), bpos*0.5)
#'     branchA <- attachBranch(branchA, branch3, ifelse(shapes$a[i] >= 0,-30,30), bpos*0.6)
#'
#'
#'   stem <- attachBranch(stem, branchA, shapes$a[i], shapes$by[i], tht = shapes$ty[i], tx = shapes$tx[i])
#'   }
#'
#'   branchB <- stem |> mutate(x=x*1,y=y) |> skewStem(amp=0.3)
#'
#'   stem2 <-  makeStem(5,0.5,0.3,30)
#'   tree <- attachBranch(stem2, branchB, bht=4.5, tht = 11, tx=-4)
#'   tree <- attachBranch(tree, branchB, bht=5, tht = 11, tx=3)
#'
#'   crown <- tree |> subset(grepl('tip',type))
#'   crown2 <- crown[chull(x=crown$x, y=crown$y),]
#'
#'   ggplot()+
#'    geom_polygon(data=tree, aes(x=x, y=y), color='brown',fill='#99500050')+
#'    # geom_point(data=stem, aes(x=x, y=y), color='red')+
#'    geom_polygon(data=crown2, aes(x=x, y=y), color='green',fill='#00990050')+
#'     # geom_point(data=tree, aes(x=x, y=y), color='green')+
#'     coord_fixed()
#'
makeCrownShape <- function(ht.max=5, ht.min=1, crwd=2, dbh=0.3, tip=0.01, crshape=c('pyramid','dome','round','column'), n=5, bu=0.8, bl=0, opposite = FALSE){

  h <- ht.max - ht.min
  wd <- crwd/2

  s <- (0:n)/n
  i <- 1:(n+1)
  d <- 1-wd/h
  sc <- (s-d)*1/(1-d)
  #create index sequence and proportion
  shapes <- data.frame(i=i,s=s)
  #create angles needed for round crowns
  shapes <- shapes |> mutate(sc = (s-d)*1/(1-d),
                             a1=s*2*pi/4,
                             a2=(s*2-1)*2*pi/4,
                             a3=sc*2*pi/4)
  #create choices of xy coordinates
  shapes <- shapes |> mutate(x0=-1*s+1,y0=s,#pyramid
                             x1=cos(a1),y1=sin(a1),#dome
                             x2=cos(a2),y2=sin(a2),#round
                             x3=cos(a3),y3=sin(a3),#column
                             x3=ifelse(s > d,x3,1),y3=ifelse(s > d,(y3*(1-d)+d),y0))
  #weights to determine shape output as branch tip coordinates
  r <- c('pyramid','dome','round','column') %in% crshape |> as.numeric()
  shapes <- shapes |> mutate(tx = (r[1]*x0+r[2]*x1+r[3]*x2+r[4]*x3)/sum(r),
                             ty = (r[1]*y0+r[2]*y1+r[3]*y2+r[4]*y3)/sum(r))
  #normalize shape
  shapes <- shapes |> mutate(ty=h*(ty-min(ty))/(max(ty)-min(ty)), tx=wd*(tx-min(tx))/(max(tx)-min(tx)))
  #branch base
  shapes <- shapes |> mutate(bx = 0, by=(bu*h-bl*h)*s+bl*h) |> subset(select = c(i,s,tx,ty,bx,by))
  #lift branches to crown base
  shapes <- shapes |> mutate(by=by+ht.min, ty=ty+ht.min)
  #angle of branch
  shapes <- shapes |> mutate(l = pmax(dbh/2,((ty-by)^2+(tx-bx)^2)^0.5), a = 360/(2*pi)*acos((ty-by)/l))
  #branch diameter
  # shapes <- shapes |> mutate(d = dbh*pmin(l*2,(ht.max-by))/ht.max+0.01)
  shapes <- shapes |> mutate(d = dbh*pmin(l*2/ht.max,(ht.max-by)/ht.max+tip*by/ht.max)+0.01)
  if(opposite){
    shapes2 <- shapes |> mutate(a = a*-1, tx = tx*-1, i=i+0.5) |> subset(!a %in% 0)
    shapes <- shapes |> rbind(shapes2) |> arrange(i)
    shapes <- shapes |> mutate(i = 1:nrow(shapes))
  }else{
    shapes <- shapes |> mutate(o = ifelse(i/2 == floor(i/2), -1,1), a = a*o, tx = tx*o, o = NULL)
  }

  return(shapes)
}



#' Concave Hull
#'
#' @param x Vector of x coordinates.
#' @param y Vector of x coordinates.
#' @param concavity Degrees of concavity: 0 = convex, 1 = first order, 2 = second order...
#' @param curvy Adds more vertices between truce vertices for a smoother border.
#' @param mag Magnitude of curve (search deeper for vertices), representing proportion relative to distance between convex vertices.
#' @param deep Retain vertices of curve even when no vertices are found.
#'
#' @returns Vector of xy points with appropriate ordering to create an outline around a set of input xy points.
#' @export
#'
#' @examples
#' @examples df <- data.frame(
#' @examples x=c(runif(50,0,50)),
#' @examples y=c(rnorm(50,5,15)),
#' @examples s = NA)
#' @examples d1 <- cavhull(df$x,df$y, concavity = 0)
#' @examples d2 <- cavhull(df$x,df$y, concavity = 1)
#' @examples d3 <- cavhull(df$x,df$y, concavity = 2, curvy = T)
#' @examples
#' @examples ggplot()+
#' @examples geom_polygon(data=d1,aes(x=x,y=y), color='blue', fill='blue')+
#' @examples geom_polygon(data=d2,aes(x=x,y=y), color='red', fill='red')+
#' @examples geom_polygon(data=d3,aes(x=x,y=y), color='green', fill='green')+
#' @examples geom_point(data=df,aes(x=x,y=y))+
#' @examples coord_fixed()

cavhull <- function(x,y, concavity = 0, curvy = FALSE, mag = 1, deep=FALSE){
  n=5 #number of segments to search between convex faces
  df <- data.frame(x=floor(x*1000)/1000,y=floor(y*1000)/1000) |> unique()

  #convex hull ----
  check <- TRUE #stopping rule
  #find starting point at bottom of plot
  miny <- min(df$y)
  minx <- min(df[df$y==miny,]$x)
  df <- mutate(df, s = ifelse(x==minx & y==miny, 1,NA))
  x1 <- df[df$s %in% 1,]$x
  y1 <- df[df$s %in% 1,]$y
  df <- df |> mutate(l1 = ((x-x1)^2+(y-y1)^2)^0.5,
                     a1=acos((x-x1)/l1)/2/pi*360,
                     a1=ifelse(y-y1 >=0,a1,-1*a1))
  amin = min(subset(df, !s %in% 1)$a1)
  df <- df |> mutate(s = ifelse(a1 == amin & is.na(s), 0, s))
  for(i in 1:nrow(df)){
    if(check){
      x0 <- df[df$s %in% (i-1),]$x
      y0 <- df[df$s %in% (i-1),]$y
      x1 <- df[df$s %in% i,]$x
      y1 <- df[df$s %in% i,]$y
      l0 = ((x1-x0)^2+(y1-y0)^2)^0.5
      a0 = acos((x1-x0)/l0)
      a0 = ifelse(y1 - y0 >=0,a0,-1*a0)
      df <- df |> mutate(xr= x-x1,
                         yr= y-y1,
                         h=(xr^2+yr^2)^0.5,
                         a1=acos(yr/h),
                         a1=ifelse(xr >=0,a1,-1*a1),
                         a1= a1+a0,
                         xr = ifelse(h==0,0,h*sin(a1)),
                         yr = ifelse(h==0,0,h*cos(a1)),
                         xr = xr,
                         a1=asin(yr/h),
                         a1=ifelse(xr >=0,-a1,pi+a1))

      amin = min(subset(df, !s %in% c(i-1,i) )$a1)
      lmin = min(subset(df, !s %in% c(i-1,i) & a1 %in% amin)$l1)
      df <- df |> mutate(s = ifelse(s %in% 0,NA,s))
      check <- is.na(subset(df,a1 %in% amin & l1 %in% lmin)$s)
      df <- df |> mutate(s = ifelse(a1 == amin & is.na(s) & l1 %in% lmin, i+1, s))
    }
  }
  #concave hull first degree ----
  df <- df |> mutate(s1 = s, type = 'core')

  if(concavity > 0){
    for(k in 1:concavity){ #k=1
      smax <- max(df$s, na.rm = TRUE)
      #visit each convex hull boundary and rotate to a common reference
      for(i in 1:smax){#i=2
        if(l0 > 0){
          i0 = ifelse(i == 1,smax,i-1)
          x0 <- df[df$s %in% (i0),]$x
          y0 <- df[df$s %in% (i0),]$y
          x1 <- df[df$s %in% i,]$x
          y1 <- df[df$s %in% i,]$y
          l0 <- ((x1-x0)^2+(y1-y0)^2)^0.5
          a0 = acos((x1-x0)/l0)
          a0 = ifelse(y1 - y0 >=0,a0,-1*a0)
          dfr <- vegnasis::rotate(x=df$x, y=df$y, a=a0/2/pi*360, cx=x0, cy=y0)
          df <- df |> mutate(xr= dfr$x-x0,
                             yr= dfr$y-y0,
                             xs=NA,xa=NA,ys=NA,yl0=NA,ydiff=NA,microinc=NA)
          #use wave to select closest concave points
          en <- pmax(3,floor(pmin(n,l0/5)))*3
          #deep curve
          if(deep & k == concavity){
            wave0 <- data.frame(x=(0:(en+1))/(en+1))
            wave0 <- wave0 |> mutate(a=x*2*pi,y=(cos(a)^1-1)/2*mag)
            wave <- data.frame(x=NA, y=NA, s=NA,l1=NA,a1=NA,xr=wave0$x*l0,
                               yr=wave0$y*l0,h=NA,s1=NA, type='cave',
                               xs=NA,xa=NA,ys=NA,yl0=NA,ydiff=NA,microinc=NA)
            wavr <- vegnasis::rotate(x=wave$xr, y=wave$yr, a=-a0/2/pi*360, cx=0,cy=0)
            wave <- wave |> mutate(x=wavr$x+x0,y=wavr$y+y0) |> subset(!yr >=0)
            df <- df |> rbind(rbind(wave))
          }
          df <- df |> mutate(xs = xr/l0, xa = xs*2*pi, ys = (cos(xa)^1-1)/2*mag,
                             yl0 = (yr/l0), ydiff = yl0-ys)
          curmax <- max(subset(df, xs > 0 & xs < 1)$ydiff)
          curcur <- subset(df, xs >= 0 & xs <=1  & ydiff == curmax)$ys
          currat <- ifelse(curmax == 0, 1, 1-curmax/abs(curcur))
          currat <- ifelse(currat < 0,0, ifelse(currat > 1,1,currat))
          df <- df |> mutate(ys = ys*currat, ydiff = yl0-ys)
          curmax <- max(subset(df, xs > 0 & xs < 1)$ydiff)
          curcur <- subset(df, xs >  0 & xs < 1  & ydiff == curmax)$ys
          currat2 <- ifelse(curmax == 0, 1, 1-curmax/abs(curcur))
          currat2 <- ifelse(currat2 < 0,0, ifelse(currat2 > 1,1,currat2))
          df <- df |> mutate(ys = ys*currat2, ydiff = yl0-ys)
          df$microinc <- NA
          #introduce wave
          if(curvy & k == concavity){
            wave0 <- data.frame(x=(0:(en+1))/(en+1))
            wave0 <- wave0 |> mutate(a=x*2*pi,y=(cos(a)^1-1)/2*mag)
            wave <- data.frame(x=NA, y=NA, s=NA,l1=NA,a1=NA,xr=wave0$x*l0,
                               yr=wave0$y*l0*currat*currat2,h=NA,s1=NA, type='wave',
                               xs=NA,xa=NA,ys=NA,yl0=NA,ydiff=NA,microinc=NA)
            wavr <- vegnasis::rotate(x=wave$xr, y=wave$yr, a=-a0/2/pi*360, cx=0,cy=0)
            wave <- wave |> mutate(x=wavr$x+x0,y=wavr$y+y0) |> subset(!yr >=0)
            df <- df |> rbind(rbind(wave))
          }
          pickthispoint <- min(abs(subset(df, xs >= 0 & xs <=1)$ydiff))
          df <- df |> mutate(microinc = ifelse((xs >  0 & xs < 1  & round(abs(ydiff),10) %in% round(pickthispoint,10) | type %in% 'wave') & is.na(s1),xr,NA))

          df$microinc <- renumber(df$microinc)

          df <- df |> mutate(s1 = ifelse(!is.na(microinc) & is.na(s), i0+microinc/1000,s1))
          df <- df |> subset(type %in% 'core' | !is.na(s1))
        }}
      df <- df |> mutate(s = renumber(s1), s1 = s)
    }
  }
  df <- subset(df, !is.na(s), select=c(x,y,s)) |> arrange(s)
  return(df)}

#' Rotate XY Coordinates
#'
#' @param x vector of x coordinates
#' @param y vector of y coordinates
#' @param a angle in degrees
#' @param cx optional center of rotation x coordinate (default is center of point cloud)
#' @param cy optional center of rotation y coordinate (default is center of point cloud)
#'
#' @returns data frame of rotated xy coordinates
#' @export
#'
#' @examples df <- data.frame(
#' @examples x=runif(10,0,10),
#' @examples y=rnorm(10,5,5))
#' @examples df2 <-  rotate(df$x,df$y, a=2)
#' @examples plot(df$y ~ df$x)
#' @examples points(df2$y ~ df2$x, col='red')
rotate <- function(x, y, a, cx = NA, cy = NA){
  df <- data.frame(x=x,y=y)

  if(is.na(cx) | is.na(cy)){
    cx <- mean(df$x)
    cy <- mean(df$y)}

  df$y0 <- df$y-cy
  df$x0 <- df$x-cx
  df$h <- ((df$x0)^2+(df$y0)^2)^0.5
  df$a0 <- ifelse(df$h==0,0,acos(df$y0/df$h))
  a1 <- a/360*2*pi
  df$a0 <- ifelse(df$x0 >= 0,df$a0,-1*df$a0)
  xr = ifelse(df$h==0,0,df$h*sin(df$a0+a1))+cx
  yr = ifelse(df$h==0,0,df$h*cos(df$a0+a1))+cy

  rdf <- data.frame(x=xr,y=yr)
  return(rdf)
}

#' Renumber a set of numbers
#'
#' @param x vector of numbers, including no data.
#'
#' @returns vector of numbers converted to sequential integers, ignoring but maintaining no data. Used inside other functions which rearrange vertices for plotting polygons.
#' @export
#'
#' @examples
renumber <- function(x){
  n <- length(x)
  df <- data.frame(s=x, neworder = NA, ind = 1:n)
  dfsort <- df[order(df$s),]
  n2 <- length(x[!is.na(x)])
  if(n2>0){
    dfsort[!is.na(dfsort$s),]$neworder <- 1:n2
    s <- dfsort[order(dfsort$ind),]$neworder}else{s <- x}
  return(s)}

