\name{get.structure}
\alias{get.structure}
\title{Get vegetation structure}
\description{
Function estimates the vegetation structure based on differential dominance of trees, shrubs, and herbs. }
\usage{
get.structure(x, simple = TRUE)
}
\arguments{
\item{x}{Standardized vegetation plot dataframe, with columns of plot, taxon, cover, type, and ht.max.}
\item{simple}{If TRUE, the results are restricted to traditional categories: grassland, shrubland, savanna, woodland, forest, etc.
If FALSE, a more elaborate [experimental] scheme considering varying amounts of shrub and sapling cover within a main category of woodland and forest, and a consideration of actual graminoids vs. forb cover.}
}


\examples{
library(vegnasis)
data("nasis.veg")
veg <- nasis.veg |> clean.veg() |> fill.type.df() |> fill.hts.df()
get.structure <- veg |> get.structure()
}
