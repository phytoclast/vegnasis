\name{summary.ESIS}
\alias{summary.ESIS}
\title{Aggregate Summary of Cover by Stratum for Multiple Plots}
\description{
Function summarizes a set of mutltiple plots by max and minimum cover and cover weighted height. Used for populating vegetation tables in NRCS EDIT database (formerly ESIS).}
\usage{
summary.ESIS(x, breaks=c(0.5,5,15), lowerQ=0.25, upperQ=0.75)
}
\arguments{
\item{x}{Species composition data frame with standardized height, cover, and growth habit columns.}
\item{breaks}{Vector of user defined stratum breaks.}
\item{lowerQ}{Lower percentile for cover (proportion 0-1, not percentage).}
\item{upperQ}{Upper percentile for cover (proportion 0-1, not percentage).}
}


\examples{
x.spp <- soilDB::get_vegplot_species_from_NASIS_db()
x.cleaned <- clean.veg(x.spp)
x.filled <- fill.hts.df(x.cleaned)
x.ESIS <- summary.ESIS(x.filled, breaks = c(0.5, 5, 12))
}
