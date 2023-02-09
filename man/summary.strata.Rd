\name{summary.strata}
\alias{summary.strata}
\title{Summary of Cover by Stratum}
\description{
This function summarises cover by stratum membership and growth habit. Inputs require a data frame processed by clean.veg() and  user supplied vector of stratum height breaks.
}
\usage{
summary.strata(x, breaks = c(0.5, 5, 15))
}
\arguments{
\item{x}{Species composition data frame with standardized height, cover, and growth habit columns.}
\item{breaks}{Vector of user defined stratum breaks.}
}


\examples{
x.spp <- soilDB::get_vegplot_species_from_NASIS_db()
x.cleaned <- clean.veg(x.spp)
x.filled <- fill.hts.df(x.cleaned)
x.stratsum <- summary.strata(x.filled, breaks = c(0.5, 5, 15))
}
