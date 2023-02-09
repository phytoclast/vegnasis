\name{clean.veg}
\alias{clean.veg}
\title{Clean vegetation data imported from NASIS}
\description{
This function takes veg plot imported from NASIS and consolidates the redundant cover and height fields.

}
\usage{
clean.veg(x)
}
\arguments{
\item{x}{Species composition data frame derived from a soilDB NASIS import function.}
}


\examples{
x.spp <- soilDB::get_vegplot_species_from_NASIS_db()
x.cleaned <- clean.veg(x.spp)
}
