\name{fill.type}
\alias{fill.type}
\title{Fill missing NASIS plant types}
\description{
Lookup the NASIS life form "type" according to default growth habit of each taxon. Include column of existing types if existing types are to be preserved while filling in only missing values. Output is a vector that can be used to populate the plant "type"" column.

}
\usage{
fill.type(taxa, type=NA)
}
\arguments{
\item{taxa}{Vector of taxon names.}
\item{type}{Vector of existing NASIS plant types (life forms). If left blank, output will ignore existing values, and replace with defaults.}
}


\examples{
veg.raw <- soilDB::get_vegplot_species_from_NASIS_db()
veg <- clean.veg(veg.raw)
veg <- fill.hts.df(veg)
veg$type <- fill.type(veg$taxon, veg$type)
}
