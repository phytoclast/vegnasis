\name{pre.fill.veg}
\alias{pre.fill.veg}
\title{Fill in missing columns with blanks}
\description{
Several vegnasis functions require data frames with a predefined standard set of column names to operate. This function processes a data frame, retaining existing standardized columns, removing non-standard columns, and establishing missing standardized columns with missing data. Prior to using this function, steps are initially taken to manually assign existing data to standardized column names as much as possible, whereas this function builds the remaining missing structure to allow subsequent vegnasis functions to operate. Units of measurement are assumed to be SI for the purposes of analysis and compatibility with vegetation science literature. Conversions to USC units, if desired, should take place only as a final step after analysis.

"plot" = Unique identifer for plot.

"label" = Convenient contextual name for plot.

"symbol"= USDA PLANTS symbol.

"taxon" = Species (binomial), and sometimes genus, subspecies, or varieties.

"type" = Growth habit or form such as 'tree', 'shrub/vine', 'grass/grasslike', 'forb', 'moss', etc.

"nativity" = 'native' or 'introduced' status within the geographic area or the plot.

"cover" = crown or canopy cover (not the same as foliar cover).

"stratum.min" = minimum height range (m) of stratum considered to be occupied by this taxon.

"stratum.max" = maximum height range (m) of stratum considered to be occupied by this taxon.

"crown.min" = bottom height (m) of live canopy for members of this taxon occupying this stratum.

"crown.max" = top height (m) of live canopy for members of this taxon occupying this stratum.

"diam.min" = mimimum diameter (cm) of trees measured (usually at "breast height", 1.37 or 1.4 m above the grown, a.k.a. 'DBH').

"diam.max"= maximum diameter (cm) of trees measured (usually at "breast height", 1.37 or 1.4 m above the grown, a.k.a. 'DBH').

'diam' = mean diameter (cm) of trees measured using a variable radius plot (BA prism) or quadratic mean diameter of all trees within a fixed plot. Defaults to a mean of the maximum and minimum diameter if not all trees were measured.

"BA" = basal area (square meters per hectare).

}
\usage{
pre.fill.veg(x)
}
\arguments{
\item{x}{User developed data frame with a variable number standard and non-standard column names.}
}


\examples{
 #Example data created to look as if imported from random csv file.
 obsite <- c('plot1','plot1','plot1', 'plot2', 'plot2')
 obsspp <- c('Acer rubrum','Pinus strobus','Pteridium aquilinum', 'Lindera benzoin', 'Trillium grandiflorum')
 abund <- c(80,10,30,10,10)
 mydata <- data.frame(obsite=obsite, obsspp=obsspp, abund=abund)

 #Identify columns containing data cooresponding to standard column names.
 mydata <- mydata |> mutate(taxon=obsspp, cover=abund, plot=obsite)
 veg <- mydata |> pre.fill.veg()
 }
