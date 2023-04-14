\name{make.plot.matrix}
\alias{make.plot.matrix}
\title{Make plot matrix}
\description{
This function takes a standardized vegetation plot dataframe and converts it to a community matrix that can be converted to a distance matrix to be analized by functions in the vegan package such as creating a distance matrix.
}
\usage{
make.plot.matrix(x, tr = NA, rc = TRUE, nr=FALSE)
}
\arguments{
\item{x}{Standardized vegetation plot dataframe, with columns of plot, taxon, and cover.}
\item{tr}{Transformation to apply to cover values. Choices include: "log" for log base 10, "sqrt" for square root, and nothing for no transformation.}
\item{rc}{Option for relative cover to adjust for sparse vegetation cover (TRUE) or keep as absolute cover (FALSE).}
\item{nr}{Option to normalize species by their highest abundance to amplify the signal of rare species (TRUE) or keep as absolute/relative cover (FALSE).}
}


\examples{
veg.raw <- vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
m <- make.plot.matrix(veg, tr = 'log')
d = vegan::vegdist(m, method='bray')
t <- cluster::agnes(d, method = 'ward')|> as.hclust()
ape::plot.phylo(ape::as.phylo(t))
t <- optpart::flexbeta(d, beta = -0.2)|> as.hclust()
ape::plot.phylo(ape::as.phylo(t))

}
