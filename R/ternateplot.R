
#' Transform to ternary xy coordinates
#'
#' This function transforms three way composition to ternary xy coordinates for ploting onto a tiangle. The inputs need only the first two axis (a is 100% at top corner, b is 100% at lower right corner). The third axis is assumed to be the difference from the first two axes and 100%. If the three axis do not add up to 100%, they must be balanced with the toabc() function. Tree and shrub data should be transformed using the tovtern() function.
#'
#' @param a Composition from bottom to top corner of triangle.
#' @param b Composition from upper left to lower right corner of triangle.
#'
#' @returns xy coordinates for plotting onto ternary graph.
#' @export
#'
#' @examples
totriangle <- function(a=NA,b=NA){
  df <- data.frame(a=a,b=b)
  df$t <- 100-df$a
  df$p <- df$b/df$t
  df$x <- ifelse(df$a == 100,0,(df$p-0.5)*df$t*2/(3^0.5))
  df$y <- df$a-100/3
  df <- subset(df, select = c(x,y))
  return(df)
}


toabc <- function(a=NA,b=NA,c=NA){
  df <- data.frame(a=a,b=b,c=c)
  df$c <- ifelse(is.na(df$c), 100-df$a-df$b, df$c)
  df$b <- ifelse(is.na(df$b), 100-df$a-df$c, df$b)
  df$a <- ifelse(is.na(df$a), 100-df$c-df$b, df$a)
  df$negs <- pmin(0,df$c)
  df$c <- df$c-df$negs
  df$b <- df$b+df$negs
  df$negs <- pmin(0,df$a)+pmin(0,df$b)+pmin(0,df$c)
  df$c <- df$c-df$negs
  df$b <- df$b-df$negs
  df$a <- df$a-df$negs
  df$total <- df$a+df$b+df$c
  df$c <- 100*df$c/df$total
  df$b <- 100*df$b/df$total
  df$a <- 100*df$a/df$total
  df <- subset(df, select = c(a,b,c))
  return(df)
}

#' Transform tree and shrub composition data to ternary xy coordinates.
#'
#' Transforms tree and shrub composition data to ternary xy coordinates. Aggregates the total tree cover, and the total woody plant cover, then derives the shrub cover by subtracting tree cover from shrub cover (shrubs only counted as cover in the portion where trees are not overhead.
#'
#' @param veg A vegetation plot data frame with key information on habit and plant hight by species.
#'
#' @returns Data frame rendering the xy coordinates to plot on ternary graph.
#' @export
#'
#' @examples
tovtern <- function(veg){
  veg <- veg  |> mutate(tree = ifelse(type %in% c('tree', 'shrub/vine') & ht.max > 5,cover,0),
                        woody = ifelse(type %in% c('tree', 'shrub/vine'),cover,0))

  veg.tern <- veg |> group_by(plot, label) |> summarise(tree = cover.agg(tree), woody = cover.agg(woody), shrub = woody - tree, open = 100 - woody)
  veg.tern <- veg.tern |> cbind(totriangle(a=veg.tern$tree, b=veg.tern$shrub)) |> as.data.frame()
  return(veg.tern)
}

#' Make ternary graph for vegetation structure
#'
#' @param axislabs Vector of three axis labels for left, right, and bottom sides, respectively.
#' @param background Should color background with predefined structural categories be displayed?
#' @param grid Should grid lines for every 10 percent be displayed?
#' @param num Should grid values along axes be labeled?
#'
#' @returns ggplot object onto which additional 3-way compositions may be graphed, assuming that their composition is transformed to triangle coordinates.
#' @export
#'
#' @examples #load sample data
#' @examples siteass <- vegnasis::siteass20250414
#' @examples sites <- vegnasis::sites20250414
#' @examples veg.raw <- vegnasis::veg.raw20250414
#' @examples vegplot <- vegnasis::vegplot20250414
#' @examples #select example plots
#' @examples thesesites <- subset(siteass, usiteassocid %in% 'DSP-F094AB019MI-2024')
#' @examples sites <- subset(sites, usiteid %in% thesesites$usiteid)
#' @examples #get related information about the plots
#' @examples landuse <- data.frame(siteobsiid=sites$siteobsiid, landuse = sites$commphasename)
#' @examples landuse <- landuse |> left_join(data.frame(siteobsiid=vegplot$siteobsiid, plot = vegplot$vegplotid)) |> subset(!is.na(plot) & !is.na(landuse))
#' @examples #fill in missing height and growth habit data
#' @examples veg <- clean.veg(veg.raw)|> subset(!is.na(taxon)) |> fill.type.df() |> fill.hts.df()
#' @examples #Aggregate total tree and shrub cover then transform to xy coordinates on triangle (shrub values are subtracted from aggregate woody plant totals so that values add up to 100%).
#' @examples veg.tern <- tovtern(veg) |> inner_join(landuse)
#' @examples #plot onto triangle
#' @examples vtern(background = T)+
#' @examples geom_point(data=veg.tern, aes(x=x, y=y, shape = landuse), size=3)+
#' @examples geom_text(data=veg.tern, aes(x=x, y=y, label=label), vjust=-1, hjust=1, size=2)+
#' @examples scale_shape_manual(values=c(17, 16, 8))+
#' @examples theme(legend.text = element_text(size = 8),legend.position='left')
vtern <- function(axislabs = c('Tree', 'Shrub', 'Herb'),background=F, grid=T, num=T){

  adot <- data.frame(a=c(0,0,10,10,20,20,30,30,40,40,50,50,60,60,70,70,80,80,90,90,100,100),
                     b=c(0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100))

  adot <- data.frame(label = c('0','10','20','30','40','50','60','70','80','90','100'),
                     a=c(0,10,20,30,40,50,60,70,80,90,100),
                     b=c(0,0,0,0,0,0,0,0,0,0,0))
  bdot <- data.frame(label = c('0','10','20','30','40','50','60','70','80','90','100'),
                     a=c(100,90,80,70,60,50,40,30,20,10,0),
                     b=c(0,10,20,30,40,50,60,70,80,90,100))
  cdot <- data.frame(label = c('0','10','20','30','40','50','60','70','80','90','100'),
                     a=c(0,0,0,0,0,0,0,0,0,0,0),
                     b=c(100,90,80,70,60,50,40,30,20,10,0))

  adot <- toabc(adot$a,adot$b) |> cbind(label=adot$label)
  bdot <- toabc(bdot$a,bdot$b) |> cbind(label=bdot$label)
  cdot <- toabc(cdot$a,cdot$b) |> cbind(label=cdot$label)

  adot <- totriangle(adot$a,adot$b)|> cbind(label=adot$label)
  bdot <- totriangle(bdot$a,bdot$b)|> cbind(label=bdot$label)
  cdot <- totriangle(cdot$a,cdot$b)|> cbind(label=cdot$label)

  adot <- adot |> mutate(xx=x-3, yy=y)
  bdot <- bdot |> mutate(xx=x+1.5, yy=y+1.5)
  cdot <- cdot |> mutate(xx=x+1.5, yy=y-2.5)

  axislab <- data.frame(label=axislabs,
                        a=c(50,50,-10),b=c(-10,60,60), angle=c(60,-60,0))
  axislab <- axislab |> cbind(totriangle(axislab$a, axislab$b))

  s1 <- data.frame(open = c(35,0,0),
                   tree = c(65,100,65),
                   shrub = c(0,0,35), group="1 forest")

  s2 <- data.frame(open = c(25,25,0,0),
                   tree = c(10,65,65,10),
                   shrub = c(65,10,35,90), group="2 dense scrubby woodland")

  s3 <- data.frame(open = c(80,25,25),
                   tree = c(10,65,10),
                   shrub = c(10,10,65), group="3 open scrubby woodland")

  s4 <- data.frame(open = c(90,35,25,80),
                   tree = c(10,65,65,10),
                   shrub = c(0,0,10,10), group="4 open woodland/savanna")

  s5 <- data.frame(open = c(25,25,0,0),
                   tree = c(0,10,10,0),
                   shrub = c(75,65,90,100), group="5 shrub thicket")

  s6 <- data.frame(open = c(90,80,25,25),
                   tree = c(0,10,10,0),
                   shrub = c(10,10,65,75), group="6 open shrubland")

  s7 <- data.frame(open = c(100,90,80,90),
                   tree = c(0,10,10,0),
                   shrub = c(0,0,10,10), group="7 open grassland")

  str.polys = rbind(s1,s2,s3,s4,s5,s6,s7)
  str.polys <- totriangle(str.polys$tree, str.polys$shrub) |> cbind(group=str.polys$group)

  gp <-  ggplot()+theme_void()+coord_fixed()
  if(grid){
    for(i in 1:10){
      aline <- rbind(adot[i+1,],bdot[11-i,])
      bline <- rbind(bdot[i+1,],cdot[11-i,])
      cline <- rbind(cdot[i+1,],adot[11-i,])
      gp <-  gp + geom_line(data=aline, aes(x=x,y=y),linewidth = 0.1, color='darkgray')+
        geom_line(data=bline, aes(x=x,y=y),linewidth = 0.1, color='darkgray')+
        geom_line(data=cline, aes(x=x,y=y),linewidth = 0.1, color='darkgray')
    }
  }
  gp <-  gp + geom_line(data=adot, aes(x=x,y=y),linewidth = 1)+
    geom_line(data=bdot, aes(x=x,y=y),linewidth = 1)+
    geom_line(data=cdot, aes(x=x,y=y),linewidth = 1)
  if(num){
    gp <- gp + geom_text(data=adot[2:11,], aes(x=xx,y=yy,label=label))+
      geom_text(data=bdot[2:11,], aes(x=xx,y=yy,label=label), angle=60)+
      geom_text(data=cdot[2:11,], aes(x=xx,y=yy,label=label), angle=-60)
  }
  gp <-  gp + geom_text(data=axislab, aes(x=x,y=y,label=label, angle=angle))

  if(background){
    gp <- gp  +geom_polygon(data=str.polys, aes(fill=group, x=x, y=y),alpha=0.5,size=0.1, color='black') +
      scale_fill_manual(values=c('darkcyan','darkgreen','green','yellowgreen','red','orange','yellow')) +
      theme(legend.position=c(0,1),legend.justification=c(0,1)) +
      labs(fill="Vegetation Structure")}
  return(gp)
}
