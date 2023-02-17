\name{harmonize.taxa}
\alias{harmonize.taxa}
\title{Harmonize taxa}
\description{
This function synonymizes taxa with BONAP or Kew's Plants of the World Online circa 2022 and returns a binomial. This ensures that aggregating functions work off a shared taxonomic backbone. An option exists to replace commonly misapplied Old World binomials: Equisetum hyemale, Athyrium filix-femina, Cypripedium calceolus, and Osmunda regalis, with: Equisetum prealtum, Athyrium angustum, Cypripedium parviflorum, and Osmunda spectabilis, respectively.

}
\usage{
harmonize.taxa(taxa, fix = FALSE)
}
\arguments{
\item{taxa}{Plant name as recorded in original plot data.}
\item{fix}{Optional fix of commonly misapplied Old World binomials to ensure they are assigned to North American species.}
}


\examples{
veg <- vegnasis::nasis.veg |> clean.veg()
veg$taxon <- harmonize.taxa(veg$taxon)
}
