#' Draw plant profile diagram of vegetation
#'
#' This function takes a processed set of strata from one vegetation plot record and displays a generic drawing of plants according to size and density in the vegetation.
#'
#' @param plants Processed strata with plant templates.
#' @param ytrans Y-axis transformation. Default is 'identity' (no transformation). For tall vegetetation a 'sqrt' (or 'pseudo_log' which allows zeros) may be needed to emphasize short understory plants.
#' @param yratio Sets aspect ratio of horizonal and vertical axis. A value greater than 1 will stretch the vegetation to appear taller and narrower. A value between 5 and 10 is needed to compesate for y-axis transformation.
#' @param units Sets the units of measure. Default is meters ("m"). Feet is an alternative ("ft").
#' @param skycolor Sets the color of the sky background (use named value like "white" or a hexcode.
#' @param fadecolor Sets the color of the background haze into which the forground color is blended to simulate depth.
#' @param gridalpha A number from 0 to 1 to vary the visibility (opacity) of the background scale grid. Use 0 for no grid.
#' @param groundcolor Sets the color of the bare ground.
#' @param xlim A vector setting the maximum and minimum limits of the x-axis of the graph (meters only). Default is 0-50 m.
#' @param ylim A vector setting the maximum and minimum limits of the y-axis (or "z"" for up and down) of the graph (meters only). Default is from -1 to 5 m above tree highest vegetation.
#' @param xticks The interval for major grid lines along the x-axis, expressed in prescribed units of measure (default is 5 m).
#' @param yticks The interval for major grid lines along the y-axis, expressed in prescribed units of measure (default is 5 m).
#' @param xslope slope in the X-axis.(across the screen)
#' @param yslope slope in the y-axis (into the screen)
#' @param xperiod distance between peaks in undulating slopes.
#' @param xamplitude vertical distance between highs and lows.
#' @param xphase relative phase of landscape undulation.
#' @param wt depth to water table (m).
#' @param px Vector of break points along gradient defined with plength.
#' @param py Vector of ground elevation at each break point (should have the same number as px). This will add to any slopes or curves modifying ground elevation.
#'
#' @returns ggplot object featuring vegetation profile diagram.
#' @export
#'
#' @examples veg.raw <-  vegnasis::nasis.veg
#' @examples veg <- clean.veg(veg.raw)
#' @examples veg.select <- subset(veg,  grepl('2022MI165021.P',plot))
#' @examples plants <- grow_plants(veg.select)
#' @examples #standard aspect ratio
#' @examples veg_profile_plot(plants)
#' @examples #transformed aspect ratio
#' @examples veg_profile_plot(plants, 'sqrt', 5)
#' @examples #Set many custum parameters.
#' @examples veg_profile_plot(plants, unit='m',  skycolor = rgb(0.8,0.98,1), fadecolor = 'lightgray', gridalpha = 0.1, groundcolor = rgb(0.55,0.45,0.2))

veg_profile_plot <- function(plants, ytrans = 'identity', yratio=1, units = 'm', skycolor = "#D9F2FF80", fadecolor = "#D9F2FF", gridalpha=0.3, groundcolor="#808066", xlim=c(0,50), ylim=c(-1, zmax+5), xticks=5, yticks=5, xslope=0, yslope=0, xperiod=10, xamplitude=0, xphase=0, wt=-2,
                             px=c(), py=c()){
  require(ggplot2)

  #rearrange stems depth drawing order
  xnmax <- max(plants$xn, na.rm =TRUE)
  xnmin <- min(plants$xn, na.rm =TRUE)
  ypmax <- max(plants$yp, na.rm =TRUE)+0.01
  ypmin <- min(plants$yp, na.rm =TRUE)-0.01
  ypwid <- ypmax-ypmin
  iters = round(ypwid,0)
  ypinc <- ypwid/iters
  plants <- plants |> arrange(yp,stumpid, objid, ptord) |> mutate(zn = zn+(xp*xslope/100)+((yp-ypmin)*yslope/100)+xamplitude+xamplitude*sin(xp/xperiod*pi*2+xphase*pi*2))


  plants <- plants |> mutate(zn = zn + slopebreaks(xp, px,py))

  #implement slope
  zmax <- max(plants$zn, na.rm =TRUE)
  plants <- plants |> mutate(fill=colormixer(fill, fadecolor, round(1-1/(1+((yp-ypmin)/20)),2)),
                             color=colormixer(color, fadecolor, round(1-1/(1+((yp-ypmin)/20)),2)))

    groundline = data.frame(xn=c(xnmin:xnmax,xnmax,xnmin),
                          zn=c((xnmin:xnmax)*0,-10,-10))|>
      mutate(zn = ifelse(zn == 0,zn+xn*xslope/100+xamplitude+xamplitude*sin(xn/xperiod*pi*2+xphase*pi*2)+
                           slopebreaks(xn,px,py),zn))


    wt = data.frame(xn=c(xnmin,xnmax,xnmax,xnmin),zn=c(wt,wt,-10,-10))
  ground.A = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.8), color=groundcolor)
  ground.B = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(4/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.5), color=groundcolor)
  ground.C = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(3/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.3), color=groundcolor)
  ground.D = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(2/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.2), color=groundcolor)
  ground.E = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(1/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.1), color=groundcolor)

  ground = data.frame(groundline, fill=groundcolor, color=groundcolor)
  #round up all the colors used to correctly assign objects in alphabetical order.
  pcolor <- c(plants$color, ground$color) |> unique() |> sort()
  pfill <- c(plants$fill, ground$fill, ground.A$fill,ground.B$fill,ground.C$fill,ground.D$fill,ground.E$fill) |> unique()|> sort()

    #set unit conversions for the basis of the tickmarks
  ucf = case_when(units %in% c('feet', 'ft') ~ 0.3048,
                  units %in% c('inches', 'in') ~ 0.3048/12,
                  units %in% c('cm') ~ 0.01,
                  TRUE ~ 1)
  units = ifelse(ucf == 1, 'm',units)

  yunits = paste0('height (', units,')')
  xunits = paste0('ground distance (', units,')')
  ybreaks = seq(floor(ylim[1]/ucf/yticks)*yticks-yticks,
                floor(ylim[2]/ucf/yticks)*yticks+yticks,
                yticks)*ucf
  xbreaks = seq(floor(xlim[1]/ucf/xticks)*xticks-xticks,floor(xlim[2]/ucf/xticks)*xticks+xticks,xticks)*ucf
  yminor = seq(floor(ylim[1]/ucf-yticks),floor(ylim[2]/ucf+yticks),yticks/5)*ucf
  xminor = seq(floor(xlim[1]/ucf-xticks),floor(xlim[2]/ucf+xticks),xticks/5)*ucf
  ylabels = ybreaks/ucf
  xlabels =  xbreaks/ucf





  gp <- ggplot()+
    geom_polygon(data=ground.A, aes(x=xn,y=zn, fill=fill, color=color), alpha=1, linewidth=0.1)+
    geom_polygon(data=ground.B, aes(x=xn,y=zn, fill=fill, color=color), alpha=1, linewidth=0.1)+
    geom_polygon(data=ground.C, aes(x=xn,y=zn, fill=fill, color=color), alpha=1, linewidth=0.1)+
    geom_polygon(data=ground.D, aes(x=xn,y=zn, fill=fill, color=color), alpha=1, linewidth=0.1)+
    geom_polygon(data=ground.E, aes(x=xn,y=zn, fill=fill, color=color), alpha=1, linewidth=0.1)+
    geom_polygon(data=ground, aes(x=xn,y=zn, fill=fill, color=color), alpha=1, linewidth=0.1)
  for(i in 1:iters){#i=1 i=1:iters
    # fade = 1-(floor(((i-1)/iters)*5)/5+0.2)
    ypcut0 = ypmax - i*ypinc
    ypcut1 = ypmax - (i-1)*ypinc
    plants0 <- plants |> subset(yp > ypcut0 & yp <= ypcut1)
    stems0 <- plants0 |> subset(obj %in% 'stem')
    crowns0 <- plants0 |> subset(obj %in% c('crown','herb'))
    gp = gp+
      geom_polygon(data=stems0, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
      geom_polygon(data=crowns0, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)
  }
  gp = gp +
    geom_polygon(data=wt,aes(x=xn,y=zn), color='#0033FF99',fill='#33CCFF', alpha=0.33)+
    scale_fill_manual(values=pfill)+
    scale_color_manual(values=pcolor)+
    theme(legend.position = "none",

          panel.background = element_rect(fill = skycolor,
                                          colour = "black",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                          colour = rgb(0.1, 0.1, 0.1, gridalpha)),
          panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                          colour = rgb(0.1, 0.1, 0.1, gridalpha/3))
    )+
    coord_fixed(ratio = yratio, ylim=ylim,xlim=xlim, expand = FALSE)+
    scale_y_continuous(name = yunits, trans = ytrans, labels = ylabels, breaks = ybreaks, minor_breaks = yminor, limits = c(-10,zmax+5))+#
    scale_x_continuous(name = xunits ,breaks = xbreaks, labels = xlabels, minor_breaks = xminor, limits = c(xnmin-5,xnmax+5))#


  return(gp)

}


slopebreaks <- function(x, px, py){
  if(length(px)>0){
    xmin=min(x);xmax=max(x)
    y=x*0
    py <- c(py[1],py,py[length(py)])
    px <- c(xmin,px,xmax)
    n <- length(px)-1
    for(i in 1:n){
      y = ifelse(x >= px[i] & x <= px[i+1],
                 (x-px[i+1])*(py[i]-py[i+1])/(px[i]-px[i+1])+py[i+1],y)}
    return(y)}else{return(x*0)}}
