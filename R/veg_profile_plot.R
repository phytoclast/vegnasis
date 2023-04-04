veg_profile_plot <- function(plants, ytrans = 'identity', yratio=1, units = 'm', skycolor = "#D9F2FF80", fadecolor = "#D9F2FF", gridalpha=0.3, groundcolor="#808066"){
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

  ground = data.frame(xn=c(0,50,50,0),zn=c(0,0,-0.5,-0.5), fill=groundcolor, color=groundcolor)
  pcolor <- c(plants2$color, ground$color) |> unique() |> sort()
  pfill <- c(plants2$fill, ground$fill) |> unique()|> sort()

  if(units %in% c('inches', 'in')){
    yunits = paste0('height (', units,')')
    xunits = paste0('ground distance (', units,')')
    ybreaks = c(-100:(1000/5))*200*0.3048/12
    xbreaks = c(-100:(1000/5))*200*0.3048/12
    ylabels = c(-100:(1000/5))*200
    xlabels = c(-100:(1000/5))*200
  }else if  (units %in% c('feet', 'ft')){
    yunits = paste0('height (', units,')')
    xunits = paste0('ground distance (', units,')')
    ybreaks = c(-100:(500/5))*20*0.3048
    xbreaks = c(-100:(500/5))*20*0.3048
    ylabels = c(-100:(500/5))*20
    xlabels = c(-100:(500/5))*20
  }else if  (units %in% c('cm')){
    yunits = paste0('height (', units,')')
    xunits = paste0('ground distance (', units,')')
    ybreaks = c(-100:(500/5))*500*0.01
    xbreaks = c(-100:(500/5))*500*0.01
    ylabels = c(-100:(500/5))*500
    xlabels = c(-100:(500/5))*500
  }else{
    yunits = 'height (m)'
    xunits = 'ground distance (m)'
    ybreaks = c(-100:(500/5))*5
    xbreaks = c(-100:(500/5))*5
    ylabels = c(-100:(500/5))*5
    xlabels = c(-100:(500/5))*5}

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
    coord_fixed(ratio = yratio)+
    scale_y_continuous(name = yunits, trans = ytrans, labels = ylabels, breaks = ybreaks, minor_breaks = ybreaks/5, limits = c(-0.5,zmax+5))+
    scale_x_continuous(name = xunits ,breaks = xbreaks, labels = xlabels, minor_breaks =xbreaks/5, limits = c(xnmin-5,xnmax+5))

  }
