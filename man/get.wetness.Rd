\name{get.wetness}
\alias{get.wetness}
\title{Get vegetation plot mean wetness indicator status}
\description{
Function takes a cleaned vegetation plot data fram with plot, taxon, and cover columns, and user specified region, and returns a mean hydric indicator status, ranging from 0 (all upland) to 1 (all obligate wetland).

Army corps regions:
'AGCP' = Atlantic and Gulf Coastal Plain;
'AW' = Arid West;
'CB' = Caribbean;
'EMP' = Eastern Mountains and Piedmont;
'GP' = Great Plains;
'HI' = HI;
'MW' = Midwest;
'NCNE' = Northcentral & Northeast;
'AK' = Alaska;
'WMVC' = Western Mountains, Valleys, and Coast.
}
\usage{
get.wetness(x, region = 'NCNE')
}
\arguments{
\item{x}{Species composition data frame cleaned with clean.veg() function.}
\item{region}{US Army Corps Region where indicator status is prioritized.}
}


\examples{
library(vegnasis)
data("nasis.veg")
veg <- nasis.veg |> clean.veg() |> fill.type.df() |> fill.hts.df()
association <- veg |> get.assoc()
wetland <- veg |> get.wetness() |> mutate(wetness = ifelse(wetness > 0.50, 'wet',''))
structure <-  veg |> get.structure()
association <- association |> left_join(wetland) |> left_join(structure)
}
