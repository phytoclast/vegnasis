\name{fill.hts.df}
\alias{fill.hts.df}
\title{Consolidate and fill missing plant heights}
\description{
Function consolidates recorded plant height data and fills in missing values based on taxon or growth habit depending on what type of height data is missing. This function requires a data frame with standardized column names.

}
\usage{
fill.hts.df(df)
}
\arguments{
\item{df}{Species composition data frame cleaned with clean.veg() function.}
}


\examples{
x.spp <- soilDB::get_vegplot_species_from_NASIS_db()
x.cleaned <- clean.veg(x.spp)
x.filled <- fill.hts.df(x.cleaned)
}
