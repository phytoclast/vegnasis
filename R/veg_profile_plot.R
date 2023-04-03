veg_profile_plot <- function(plants, ytrans = 'identity', yratio=1){
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
    mutate(fill=colormixer(fill, "#D9F2FF", 0.8), color=colormixer(color, "#D9F2FF", 0.8))
  crowns2 <- plants |> subset(depth %in% 'D' & obj %in% c('crown','herb')) |>
    mutate(fill=colormixer(fill, "#D9F2FF", 0.6), color=colormixer(color, "#D9F2FF", 0.6))
  crowns3 <- plants |> subset(depth %in% 'C' & obj %in% c('crown','herb')) |>
    mutate(fill=colormixer(fill, "#D9F2FF", 0.4), color=colormixer(color, "#D9F2FF", 0.4))
  crowns4 <- plants |> subset(depth %in% 'B' & obj %in% c('crown','herb'))|>
    mutate(fill=colormixer(fill, "#D9F2FF", 0.2), color=colormixer(color, "#D9F2FF", 0.2))
  crowns5 <- plants |> subset(depth %in% 'A' & obj %in% c('crown','herb'))

  stems1 <- plants |> subset(depth %in% 'E' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, "#D9F2FF", 0.8), color=colormixer(color, "#D9F2FF", 0.8))
  stems2 <- plants |> subset(depth %in% 'D' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, "#D9F2FF", 0.6), color=colormixer(color, "#D9F2FF", 0.6))
  stems3 <- plants |> subset(depth %in% 'C' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, "#D9F2FF", 0.4), color=colormixer(color, "#D9F2FF", 0.4))
  stems4 <- plants |> subset(depth %in% 'B' & obj %in% 'stem')|>
    mutate(fill=colormixer(fill, "#D9F2FF", 0.2), color=colormixer(color, "#D9F2FF", 0.2))
  stems5 <- plants |> subset(depth %in% 'A' & obj %in% 'stem')

  plants2 <- rbind(crowns1,crowns2,crowns3,crowns4,crowns5,stems1,stems2,stems3,stems4,stems5)

  # ggplot()+
  #   geom_point(aes(x=1,y=1))+
  #   theme(panel.background = element_rect(fill = rgb(0.4,0.6,0.5)))

  pcolor <- plants2$color |> unique() |> sort()
  pfill <- plants2$fill |> unique()|> sort()



  ggplot() +
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
          panel.background = element_rect(fill = rgb(0.85,0.95,1,0.5),
                                          colour = "black",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                          colour = rgb(0.1, 0.1, 0.1, 0.3)),
          panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                          colour = rgb(0.1, 0.1, 0.1, 0.1))
    )+
    coord_fixed(ratio = yratio)+
    scale_y_continuous(name = 'height (m)', trans = ytrans, breaks = c(-20:(120/5))*5,minor_breaks = c(-20:(120)), limits = c(0,zmax+5))+
    scale_x_continuous(name = 'ground distance (m)',breaks = c(-10:(120/5))*5, minor_breaks = c(-10:(120)), limits = c(xnmin-5,xnmax+5))
}
