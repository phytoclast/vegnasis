\name{link.taxonomy}
\alias{link.taxonomy}
\title{Link to APG Taxonomy}
\description{
This function parses genus from taxa and links to family according to APG IV taxonomy (vascular plants only). If binomial is phylogenetically inconsistent with the type species of the genus, the family will be incorrect. Harmonizing taxa first is recommended.
}
\usage{
link.taxonomy(taxa, taxrank = 2)
}
\arguments{
\item{taxa}{Plant name as recorded in plot data.}
\item{taxrank}{Taxonomic rank above species to return (i.e. genus, family, order, superorder, subclass, class, superclass, subphylum, phylum)}
}

\examples{
veg <- vegnasis::nasis.veg |> clean.veg()
veg$taxon <- harmonize.taxa(veg$taxon)
veg$genus <- link.taxonomy(veg$taxon, taxrank = 1)
veg$family <- link.taxonomy(veg$taxon, taxrank = 2)
}
