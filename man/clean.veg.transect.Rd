\name{clean.veg.transect}
\alias{clean.veg.transect}
\title{Clean vegetation data imported from NASIS transects}
\description{
This function takes veg plot imported from NASIS transects and consolidates the redundant cover and height fields.

}
\usage{
clean.veg.transect(x)
}
\arguments{
\item{x}{Species composition data frame derived from a soilDB NASIS import function, from the transect subtable.}
}


\examples{
veg.raw <- soilDB::get_vegplot_transpecies_from_NASIS_db()
veg <- clean.veg.transect(veg.raw)
}
