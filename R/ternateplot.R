
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

axislab <- data.frame(label=c('Tree','Shrub','Herb'),
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
for(i in 1:10){
  aline <- rbind(adot[i+1,],bdot[11-i,])
  bline <- rbind(bdot[i+1,],cdot[11-i,])
  cline <- rbind(cdot[i+1,],adot[11-i,])
  gp <-  gp + geom_line(data=aline, aes(x=x,y=y))+
    geom_line(data=bline, aes(x=x,y=y))+
    geom_line(data=cline, aes(x=x,y=y))
}
gp <-  gp + geom_line(data=adot, aes(x=x,y=y))+
  geom_line(data=bdot, aes(x=x,y=y))+
  geom_line(data=cdot, aes(x=x,y=y))

gp <- gp + geom_text(data=adot, aes(x=xx,y=yy,label=label))+
  geom_text(data=bdot, aes(x=xx,y=yy,label=label), angle=60)+
  geom_text(data=cdot, aes(x=xx,y=yy,label=label), angle=-60)+
  geom_text(data=axislab, aes(x=x,y=y,label=label, angle=angle))

 gp  +
   geom_polygon(data=str.polys, aes(fill=group, x=x, y=y),alpha=0.5,size=0.1, color='black') +
   # geom_point(data=veg.tern, aes(x=x, y=y))+
   scale_fill_manual(values=c('darkcyan','darkgreen','green','yellowgreen','red','orange','yellow')) +
   theme(legend.position=c(0,1),legend.justification=c(0,1)) +
   labs(fill="Vegetation Structure")

