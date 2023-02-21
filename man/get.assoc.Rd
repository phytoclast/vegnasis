\name{get.assoc}
\alias{get.assoc}
\title{Get plant association}
\description{
Function takes a standardize plot data frame and determines the most dominant species and constructs an empirical association name. Slashes separate strata, and hyphens separate species within the same stratum. Species are ranked according to cover. Up to 3 species per stratum and up to 5 species total can be listed, depending on relative dominance (fewer species are listed if cover is low relative to the most abundant species)}
\usage{
get.assoc(x)
}
\arguments{
\item{x}{Standardized vegetation plot dataframe, with columns of plot, taxon, cover, type, and ht.max.}
}


\examples{
library(vegnasis)
data("nasis.veg")
veg <- nasis.veg |> clean.veg() |> fill.type.df() |> fill.hts.df()
association <- veg |> get.assoc()
}
