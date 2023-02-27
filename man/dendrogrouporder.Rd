\name{dendrogrouporder}
\alias{dendrogrouporder}
\title{Reorder dendrogram branches}
\description{
This function reorders branches and groups based on order of nestedness. Requires 'vegan', 'cluster', and 'ape' packages.
}
\usage{
dendrogrouporder(t,groups)
}
\arguments{
\item{t}{Dendrogram/tree produced by a cluster analysis.}
\item{groups}{Groups from cutree function.}
}


\examples{
veg.raw <- vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
m <- make.plot.matrix(veg, tr = 'log')
d = vegan::vegdist(m, method='bray')
t <- cluster::agnes(d, method = 'ward')|> as.hclust()
k = 3
groups <- cutree(t, k = k)
groups <- dendrogrouporder(t, groups)
a <- 'Wexford Legacy Data'
export.dendro(a,d,t,groups)


}
