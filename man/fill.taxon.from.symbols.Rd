\name{fill.taxon.from.symbols}
\alias{fill.taxon.from.symbols}
\title{Fill Taxon from USDA Plants Symbols}
\description{
This function fills in missing plant names from USDA symbols if they exist.
}
\usage{
fill.taxon.from.symbols(symbol, taxa=NA)
}
\arguments{
\item{symbol}{Plant Symbol.}
\item{taxa}{Optional pre-existing taxon names if only missing names are to be filled.}
}


\examples{
 symbols = c('ACRU','QUAL','ANGE')
 taxon <- fill.taxon.from.symbols(symbols)
}
