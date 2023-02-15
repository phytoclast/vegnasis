\name{harmonize.taxa}
\alias{harmonize.taxa}
\title{Harmonize taxa}
\description{
This function synonymizes taxa with BONAP or Kew's Plants of the World Online circa 2022 and returns a binomial. This ensures that aggregating functions work off a shared taxonomic backbone.

}
\usage{
harmonize.taxa(taxa)
}
\arguments{
\item{taxa}{Plant name as recorded in original plot data.}
}


\examples{
veg <- vegnasis::nasis.veg |> clean.veg()
veg$taxon <- harmonize.taxa(veg$taxon)
}
