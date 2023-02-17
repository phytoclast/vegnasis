\name{clean.veg.log}
\alias{clean.veg.log}
\title{Clean vegetation data imported from custom MSAccess VegLog database}
\description{
This function process the site and observed species tables imported from custom MSAccess VegLog database and consolidates 4 strata of cover and height fields into single cover column and height columns.

}
\usage{
clean.veg.log(obssites, obstaxa)
}
\arguments{
\item{obssites}{Site observation table.}
\item{obstaxa}{Species observation table.}
}


\examples{
obssites <- vegnasis::obs
obstaxa <- vegnasis::obsspp
veg <- clean.veg.log(obssites, obstaxa)
veg <- veg |> fill.type.df() |> fill.nativity.df() |> mutate(symbol = fill.usda.symbols(taxon))
}
