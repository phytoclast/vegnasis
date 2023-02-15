\name{fill.usda.symbols}
\alias{fill.usda.symbols}
\title{Fill USDA Plants Symbols}
\description{
This function fills in missing USDA PLANTS symbols, assuming that the concept behind the names being used are not misapplied homonyms (auct non). If existing symbols are provided (optional), priority is given to existing symbols, while only missing data is replaced.

}
\usage{
fill.usda.symbols(taxa, symbol=NA)
}
\arguments{
\item{taxa}{Plant name as recorded in original plot data.}
}


\examples{
veg <- vegnasis::nasis.veg |> clean.veg()
veg$symbol <- fill.usda.symbols(veg$taxon, veg$symbol)
}
