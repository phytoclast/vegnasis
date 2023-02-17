\name{cover.agg}
\alias{cover.agg}
\title{Aggregate crown cover}
\description{
This function aggregates cover among taxa of a given stratum or guild, assuming random crown distributions, and where totals cannot not exceeding 100 percent. This function is called within many vegetation summary functions. Essentially the same as a method suggested by an equation in Appendex K of the US National Vegetation Classification (USNVC, 2008).
}
\usage{
cover.agg(x)
}
\arguments{
\item{x}{Cover of individual taxa or strata to aggregate.}
}


\examples{
x <- c(5,5,10,20)
y <- cover.agg(x)
}
