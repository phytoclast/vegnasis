\name{grow_plants}
\alias{grow_plants}
\title{Prepare strata for drawing plant profiles.}
\description{
This function takes a standardized vegetation dataframe and fills in details neccesary to construct diagramatic representation of the plants within a vegetation. Among the details are assumptions regarding missing stem diameters, crown widths, and density. Plant dimension and placement are randomized slightly after initial values are established. For best results, be sure to limit to one plot record at a time. Dimesions of the plot sets up the number of plants needing to be planted to fill the desired limits, but the xy limits themselves are set in a seperate graphing function.

}
\usage{
grow_plants(veg, plength = 50, pwidth=20)
}
\arguments{
\item{veg}{Standardized vegetation plot dataframe.}
\item{plength}{Plot length to be viewed on the x-axis.}
\item{pwidth}{Plot width to be viewed as depth into the screen from the side profile perspective (or y-axis in top-down horizonal view).}
}


\examples{
veg.raw <-  vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
veg.select <- subset(veg,  grepl('2022MI165021.P',plot))
plants <- grow_plants(veg.select)

}
