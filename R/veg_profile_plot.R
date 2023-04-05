veg_profile_plot <- function(plants, ytrans = 'identity', yratio=1, units = 'm', skycolor = "#D9F2FF80", fadecolor = "#D9F2FF", gridalpha=0.3, groundcolor="#808066", xlim=c(0,50), ylim=c(-1,NA), xticks=5, yticks=5){
  require(ggplot2)

  #rearrange stems depth drawing order
  plants <- plants |> arrange(yp,stumpid, objid, ptord)
  zmax <- max(plants$zn)
  xnmax <- max(plants$xn)
  xnmin <- min(plants$xn)
  ypmax <- max(plants$yp)
  ypmin <- min(plants$yp)
  ypwid <- ypmax-ypmin
  plants <- plants |> mutate(depth = case_when(yp < ypmin+ypwid*(1/5) ~ 'E',
                                               yp < ypmin+ypwid*(2/5) ~ 'D',
                                               yp < ypmin+ypwid*(3/5) ~ 'C',
                                               yp < ypmin+ypwid*(4/5) ~ 'B',
                                               TRUE ~ 'A'))

  crowns1 <- plants |> subset(depth %in% 'E' & obj %in% c('crown','herb')) |>
    mutate(fill=colormixer(fill, fadecolor, 0.8), color=colormixer(color, fadecolor, 0.8))
  crowns2 <- plants |> subset(depth %in% 'D' & obj %in% c('crown','herb')) |>
    mutate(fill=colormixer(fill, fadecolor, 0.6), color=colormixer(color, fadecolor, 0.6))
  crowns3 <- plants |> subset(depth %in% 'C' & obj %in% c('crown','herb')) |>
    mutate(fill=colormixer(fill, fadecolor, 0.4), color=colormixer(color, fadecolor, 0.4))
  crowns4 <- plants |> subset(depth %in% 'B' & obj %in% c('crown','herb'))|>
    mutate(fill=colormixer(fill, fadecolor, 0.2), color=colormixer(color, fadecolor, 0.2))
  crowns5 <- plants |> subset(depth %in% 'A' & obj %in% c('crown','herb'))

  stems1 <- plants |> subset(depth %in% 'E' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, fadecolor, 0.8), color=colormixer(color, fadecolor, 0.8))
  stems2 <- plants |> subset(depth %in% 'D' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, fadecolor, 0.6), color=colormixer(color, fadecolor, 0.6))
  stems3 <- plants |> subset(depth %in% 'C' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, fadecolor, 0.4), color=colormixer(color, fadecolor, 0.4))
  stems4 <- plants |> subset(depth %in% 'B' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, fadecolor, 0.2), color=colormixer(color, fadecolor, 0.2))
  stems5 <- plants |> subset(depth %in% 'A' & obj %in% 'stem')

  plants2 <- rbind(crowns1,crowns2,crowns3,crowns4,crowns5,stems1,stems2,stems3,stems4,stems5)

  ground = data.frame(xn=c(xnmin,xnmax,xnmax,xnmin),zn=c(0,0,-2,-2), fill=groundcolor, color=groundcolor)
  #round up all the colors used to correctly assign objects in alphabetical order.
  pcolor <- c(plants2$color, ground$color) |> unique() |> sort()
  pfill <- c(plants2$fill, ground$fill) |> unique()|> sort()
  #set unit coversions for the basis of the tickmarks
  ucf = case_when(units %in% c('feet', 'ft') ~ 0.3048,
                  units %in% c('inches', 'in') ~ 0.3048/12,
                  units %in% c('cm') ~ 0.01,
                  TRUE ~ 1)
  units = ifelse(ucf == 1, 'm',units)
  ylim[2] = ifelse(is.na(ylim[2]), zmax+5, ylim[2])
  yunits = paste0('height (', units,')')
  xunits = paste0('ground distance (', units,')')
  ybreaks = seq(floor(ylim[1]/ucf/yticks)*yticks-yticks,floor(ylim[2]/ucf/yticks)*yticks+yticks,yticks)*ucf
  xbreaks = seq(floor(xlim[1]/ucf/xticks)*xticks-xticks,floor(xlim[2]/ucf/xticks)*xticks+xticks,xticks)*ucf
  yminor = seq(floor(ylim[1]/ucf-yticks),floor(ylim[2]/ucf+yticks),yticks/5)*ucf
  xminor = seq(floor(xlim[1]/ucf-xticks),floor(xlim[2]/ucf+xticks),xticks/5)*ucf
  ylabels = ybreaks/ucf
  xlabels =  xbreaks/ucf

  ggplot() +
    geom_polygon(data=ground, aes(x=xn,y=zn, fill=fill, color=color), alpha=1, linewidth=0.1)+
    geom_polygon(data=stems1, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=crowns1, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=stems2, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=crowns2, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=stems3, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=crowns3, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=stems4, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=crowns4, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=stems5, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
    geom_polygon(data=crowns5, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
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
    scale_y_continuous(name = yunits, trans = ytrans, labels = ylabels, breaks = ybreaks, minor_breaks = yminor, limits = c(-5,zmax+5))+#
    scale_x_continuous(name = xunits ,breaks = xbreaks, labels = xlabels, minor_breaks = xminor, limits = c(xnmin-5,xnmax+5))#


  }
