\name{summary.crown.thickness}
\alias{summary.crown.thickness}
\title{Cover Cross Section by Height}
\description{
This function summarises crown overlap by stratum and growth habit. Inputs require a data frame processed by clean.veg and filled heights and user supplied vector of stratum height breaks.
}
\usage{
summary.crown.thickness(x, breaks = c(0.5, 5, 15))
}
\arguments{
\item{x}{Species composition data frame with standardized height, cover, and growth habit columns.}
\item{breaks}{Vector of user defined stratum breaks.}
}


\examples{
x.spp <- soilDB::get_vegplot_species_from_NASIS_db()
x.cleaned <- clean.veg(x.spp)
x.filled <- fill.hts.df(x.cleaned)
x.cross <- summary.crown.thickness(x.filled, breaks = c(0.5, 5, 15))
}
