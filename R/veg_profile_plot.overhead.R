#' Draw plant overhead diagram of vegetation
#'
#' @param plants Processed strata with overhead plant templates.
#' @param units Sets the units of measure. Default is meters ("m"). Feet is an alternative ("ft").
#' @param gridalpha A number from 0 to 1 to vary the visibility (opacity) of the background scale grid. Use 0 for no grid.
#' @param groundcolor Sets the color of the bare ground.
#' @param xlim A vector setting the maximum and minimum limits of the x-axis of the graph (meters only). Default is 0-50 m.
#' @param ylim A vector setting the maximum and minimum limits of the y-axis of the graph (meters only). Default is 15-35 m.
#' @param xticks The interval for major grid lines along the x-axis, expressed in prescribed units of measure (default is 5 m).
#'
#' @returns ggplot object featuring vegetation overhead diagram.
#' @export
#'
#' @examples veg.raw <-  vegnasis::nasis.veg
#' @examples veg <- clean.veg(veg.raw)
#' @examples veg <- subset(veg,  grepl('2022MI165021.P',plot))
#' @examples strats <- setstrats(veg, plength = 50, pwidth=20)
#' @examples stand <- setstand(strats, plength = 50, pwidth=20)
#' @examples plants <- setplants.overhead(strats, stand)
#' @examples veg_overhead_plot(plants)

veg_overhead_plot <- function(plants, units = 'm', gridalpha=0.3, groundcolor="#808066", xlim=c(0,50), ylim=c(15,35), xticks=5){
  require(ggplot2)

  #rearrange stems depth drawing order
  xnmax <- max(plants$xn, na.rm =TRUE)
  xnmin <- min(plants$xn, na.rm =TRUE)
  ynmax <- max(plants$yn, na.rm =TRUE)
  ynmin <- min(plants$yn, na.rm =TRUE)
  plants <- plants |> arrange(-stratid, stumpid, objid, ptord)
  strats <- unique(plants$stratid)

  pcolor <- c(plants$color) |> unique() |> sort()
  pfill <- c(plants$fill) |> unique()|> sort()

  #set unit conversions for the basis of the tickmarks
  ucf = case_when(units %in% c('feet', 'ft') ~ 0.3048,
                  units %in% c('inches', 'in') ~ 0.3048/12,
                  units %in% c('cm') ~ 0.01,
                  TRUE ~ 1)
  units = ifelse(ucf == 1, 'm',units)

  xunits = paste0('ground distance (', units,')')
  xbreaks = seq(floor(xlim[1]/ucf/xticks)*xticks-xticks,floor(xlim[2]/ucf/xticks)*xticks+xticks,xticks)*ucf
  xminor = seq(floor(xlim[1]/ucf-xticks),floor(xlim[2]/ucf+xticks),xticks/5)*ucf
  ylabels = xbreaks/ucf
  xlabels =  xbreaks/ucf





  gp <- ggplot()

  for(i in 1:length(strats)){#i=1
    thistrat <- strats[i]
    plants0 <- plants |> subset(stratid %in% thistrat)
    stems0 <- plants0 |> subset(obj %in% 'stem')
    crowns0 <- plants0 |> subset(obj %in% c('crown','herb'))
    gp = gp+
      geom_polygon(data=stems0, aes(x=xn,y=yn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
      geom_polygon(data=crowns0, aes(x=xn,y=yn,group=objid, fill=fill), color=NULL, alpha=1, linewidth=0.01)

  }
  gp = gp +
    scale_fill_manual(values=pfill)+
    scale_color_manual(values=pcolor)+
    theme(legend.position = "none",

          panel.background = element_rect(fill = groundcolor,
                                          colour = "black",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                          colour = rgb(0.1, 0.1, 0.1, gridalpha)),
          panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                          colour = rgb(0.1, 0.1, 0.1, gridalpha/3))
    )+
    coord_fixed( ylim=ylim,xlim=xlim, expand = FALSE)+
    scale_y_continuous(name = xunits, labels = ylabels, breaks = xbreaks, minor_breaks = xminor, limits =c(ynmin-5,ynmax+5))+#
    scale_x_continuous(name = xunits ,breaks = xbreaks, labels = xlabels, minor_breaks = xminor, limits = c(xnmin-5,xnmax+5))#


  return(gp)

}
