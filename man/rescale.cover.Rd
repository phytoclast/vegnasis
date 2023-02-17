\name{rescale.cover}
\alias{rescale.cover}
\title{Rescale taxon cover to meet expected aggregate cover}
\description{
This function re-scales ocular estimates of individual species to force them to be more coherent with a more accurate ocular estimate of total cover for the stratum. Alternatively, a set of foliar covers could be coersed to virtual canopy cover, if canopy cover of the whole stand is known. Function takes a set of individual percentages, and a single target value for expected aggregate cover. To be used when the number of taxa and stand density makes it a challenge to estimate the absolute cover of each taxon, but for which it is reasonable to estimate their relative proportions within the stand. Values change in proportion to relative cover when total cover is low. But cover values may diverge from relative cover nonlinearly as individual covers approach 100 percent, because total cover cannot exceed 100 percent.
}
\usage{
rescale.cover(taxon.cover, aggregate.cover)
}
\arguments{
\item{taxon.cover}{Cover of individual taxa or strata to aggregate.}
\item{aggregate.cover}{Expected aggregate cover.}
}


\examples{
x <- c(5,5,10,20)
z <- 50
y <- rescale.cover(x, z)
}
