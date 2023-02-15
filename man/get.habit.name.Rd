\name{get.habit.name}
\alias{get.habit.name}
\title{Get growth habit name}
\description{
This function looks up a descriptive growth habit name for a give growth habit code.

}
\usage{
get.habit.name(code,type='name')
}
\arguments{
\item{code}{Growth habit code as determined from get.habit.code().}
\item{type}{Alternative output choices: "name" = descriptive name; "ESIS" =  generalized life form name as used in ESIS and EDIT platforms of the NRCS; "stem" = extracts symbol for only the first letter representing woodiness and stature, letters H,S,T,L, and E.}
}


\examples{
veg <- vegnasis::nasis.veg |> clean.veg()
veg$habitcode <- get.habit.code(veg$taxon)
veg$habitname <- get.habit.code(veg$habitcode)
}
