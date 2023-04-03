\name{prepare_strata}
\alias{prepare_strata}
\title{Prepare strata for drawing plant profiles.}
\description{
This function takes a standardized vegetation dataframe and fills in details neccesary to construct diagramatic representation of the plants within a vegetation. Among the details are assumptions regarding missing stem diameters, crown widths, and density. Plant dimension and placement are randomized slightly after initial values are established. For best results, be sure to limit to one plot record at a time.

}
\usage{
prepare_strata(veg)
}
\arguments{
\item{veg}{Standardized vegetation plot dataframe.}
}


\examples{
veg.raw <-  vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
veg.select <- subset(veg,  grepl('2022MI165021.P',plot))
plants <- prepare_strata(veg.select)

}
