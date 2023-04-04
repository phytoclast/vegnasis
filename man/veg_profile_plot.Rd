\name{veg_profile_plot}
\alias{veg_profile_plot}
\title{Draw plant profile diagram of vegetation.}
\description{
This function takes a processed set of strata from one vegetation plot record and displays a generic drawing of plants according to size and density in the vegetation

}
\usage{
veg_profile_plot(plants, ytrans = 'identity', yratio=1, units = 'm', skycolor = "#D9F2FF80", fadecolor = "#D9F2FF", gridalpha=0.3, groundcolor="#808066")
}
\arguments{
\item{plants}{Processed strata with plant templates.}
\item{ytrans}{Y-axis transformation. Default is 'identity' (no transformation). For tall vegetetation a 'sqrt' (or 'pseudo_log' which allows zeros) may be needed to emphasize short understory plants.}
\item{yratio}{Sets aspect ratio of horizonal and vertical axis. A value greater than 1 will stretch the vegetation to appear taller and narrower. A value between 5 and 10 is needed to compesate for y-axis transformation.}
\item{units}{Sets the units of measure. Default is meters ("m"). Feet is an alternative ("ft").}
\item{skycolor}{Sets the color of the sky background (use named value like "white" or a hexcode.}
\item{fadecolor}{Sets the color of the background haze into which the forground color is blended to simulate depth.}
\item{gridalpha}{A number from 0 to 1 to vary the visibility (opacity) of the background scale grid. Use 0 for no grid.}
\item{groundcolor}{Sets the color of the bare ground.}
}


\examples{
veg.raw <-  vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
veg.select <- subset(veg,  grepl('2022MI165021.P',plot))
plants <- prepare_strata(veg.select)
#standard aspect ratio
veg_profile_plot(plants)
#transformed aspect ratio
veg_profile_plot(plants, 'sqrt', 5)
#Set many custum parameters.
veg_profile_plot(plants, unit='m',  skycolor = rgb(0.8,0.98,1), fadecolor = 'lightgray', gridalpha = 0.1, groundcolor = rgb(0.55,0.45,0.2))



}
