\name{exo.flora}
\alias{exo.flora}
\title{Get the percentage cover consisting of flora introduced to the region.}
\description{
This function determines the percentage cover consisting of flora introduced to the region (presumably since 1492).}
\usage{
exo.flora(x)
}
\arguments{
\item{x}{Data frame consisting of columns for plot, taxon, cover, and nativity.}
}


\examples{
library(vegnasis)
data("nasis.veg")
veg <- nasis.veg |> clean.veg() |> mutate(taxon = harmonize.taxa(taxon, fix=TRUE)) |> fill.nativity.df()
exo <- exo.flora(veg)
}
