\name{export.dendro}
\alias{export.dendro}
\title{Export colored dendrogram}
\description{
This function exports a dendrogram colored according to group. Requires 'vegan', 'cluster', 'ape', and 'dendextend' packages.
}
\usage{
export.dendro(a,d,t,groups, folder=NA)
}
\arguments{
\item{a}{Title of plot.}
\item{d}{Distance/similarity matrix.}
\item{t}{Dendrogram/tree produced by a cluster analysis.}
\item{groups}{Groups from cutree function.}
\item{folder}{Folder to export file relative to working directory (default is root of working directory).}
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
a = 'Vegetation of Wexford County'
folder = 'output'
export.dendro(a,d,t,groups,folder)

}
