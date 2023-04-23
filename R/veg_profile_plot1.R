veg_profile_plot1 <- function(plants, ytrans = 'identity', yratio=1, units = 'm', skycolor = "#D9F2FF80", fadecolor = "#D9F2FF", gridalpha=0.3, groundcolor="#808066", xlim=c(0,50), ylim=c(-1, zmax+5), xticks=5, yticks=5, xslope=0, yslope=0, xperiod=10, xamplitude=0){
  require(ggplot2)

  #rearrange stems depth drawing order
  xnmax <- max(plants$xn, na.rm =TRUE)
  xnmin <- min(plants$xn, na.rm =TRUE)
  ypmax <- max(plants$yp, na.rm =TRUE)+0.01
  ypmin <- min(plants$yp, na.rm =TRUE)-0.01
  ypwid <- ypmax-ypmin
  iters = round(ypwid,0)
  ypinc <- ypwid/iters
  plants <- plants |> arrange(yp,stumpid, objid, ptord) |> mutate(zn = zn+(xp*xslope/100)+((yp-ypmin)*yslope/100)+
                                                                    xamplitude+xamplitude*sin(xp/xperiod*3.141592*2)) #implement slope
  zmax <- max(plants$zn, na.rm =TRUE)
  plants <- plants |> mutate(fill=colormixer(fill, fadecolor, round(1-1/(1+((yp-ypmin)/20)),2)),
                             color=colormixer(color, fadecolor, round(1-1/(1+((yp-ypmin)/20)),2)))

  # plants <- plants |> mutate(depth = case_when(yp < ypmin+ypwid*(1/5) ~ 'A',
  #                                              yp < ypmin+ypwid*(2/5) ~ 'B',
  #                                              yp < ypmin+ypwid*(3/5) ~ 'C',
  #                                              yp < ypmin+ypwid*(4/5) ~ 'D',
  #                                              TRUE ~ 'E'))
  groundline = data.frame(xn=c(xnmin:xnmax,xnmax,xnmin),
                          zn=c((xnmin:xnmax)*0,-10,-10))|> mutate(zn = ifelse(zn == 0,zn+xn*xslope/100+
                                                                        xamplitude+xamplitude*sin(xn/xperiod*3.141592*2),zn))
  ground.A = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.8), color=groundcolor)
  ground.B = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(4/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.5), color=groundcolor)
  ground.C = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(3/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.3), color=groundcolor)
  ground.D = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(2/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.2), color=groundcolor)
  ground.E = groundline |> mutate(zn = ifelse(zn >= 0,zn+ypwid*(1/5)*yslope/100,zn), fill=colormixer(groundcolor, fadecolor, 0.1), color=groundcolor)


  # crowns1 <- plants |> subset(depth %in% 'E' & obj %in% c('crown','herb')) |>
  #   mutate(fill=colormixer(fill, fadecolor, 0.8), color=colormixer(color, fadecolor, 0.8))
  # crowns2 <- plants |> subset(depth %in% 'D' & obj %in% c('crown','herb')) |>
  #   mutate(fill=colormixer(fill, fadecolor, 0.6), color=colormixer(color, fadecolor, 0.6))
  # crowns3 <- plants |> subset(depth %in% 'C' & obj %in% c('crown','herb')) |>
  #   mutate(fill=colormixer(fill, fadecolor, 0.4), color=colormixer(color, fadecolor, 0.4))
  # crowns4 <- plants |> subset(depth %in% 'B' & obj %in% c('crown','herb'))|>
  #   mutate(fill=colormixer(fill, fadecolor, 0.2), color=colormixer(color, fadecolor, 0.2))
  # crowns5 <- plants |> subset(depth %in% 'A' & obj %in% c('crown','herb'))
  #
  # stems1 <- plants |> subset(depth %in% 'E' & obj %in% 'stem')|>
  #   mutate(fill=colormixer(fill, fadecolor, 0.8), color=colormixer(color, fadecolor, 0.8))
  # stems2 <- plants |> subset(depth %in% 'D' & obj %in% 'stem')|>
  #   mutate(fill=colormixer(fill, fadecolor, 0.6), color=colormixer(color, fadecolor, 0.6))
  # stems3 <- plants |> subset(depth %in% 'C' & obj %in% 'stem')|>
  #   mutate(fill=colormixer(fill, fadecolor, 0.4), color=colormixer(color, fadecolor, 0.4))
  # stems4 <- plants |> subset(depth %in% 'B' & obj %in% 'stem')|>
  #   mutate(fill=colormixer(fill, fadecolor, 0.2), color=colormixer(color, fadecolor, 0.2))
  # stems5 <- plants |> subset(depth %in% 'A' & obj %in% 'stem')
  #
  # plants2 <- rbind(crowns1,crowns2,crowns3,crowns4,crowns5,stems1,stems2,stems3,stems4,stems5)

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
