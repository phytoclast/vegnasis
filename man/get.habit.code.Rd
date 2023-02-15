\name{get.habit.code}
\alias{get.habit.code}
\title{Get growth habit codes}
\description{
This function looks up a default growth habit ("life form") code for a taxon. Codes are modified from Appendix E of the US National Vegetation Classification (USNVC, 2008). The first letters:

"H" = herbs (rooted forbs and graminoids inclusive);

"S" = shrubs, free standing woody plants normally < 5 m tall when mature;

"T" = trees, free standing woody plants normally > 5 m tall when mature;

"L" = lianas and woody vines normally rooted in the ground and dependent on other plants for structural support;

"E" = epiphytes normally not rooted in the ground, established in the branches or trunks of other plants (mostly trees) -- can be either parasitic or autotrophic, and includes both woody and herbaceous examples;

"N" = Nonvascular autotrophs, including bryophytes, lichens, and algae. Most of the time when nonvascular autotrophs are listed in a plot, it is due to their dominance in the ground stratum, however any positive height attributed to the plants should be assumed to be indicate an epiphytic position within the shrub tree stratum, and not the stature of the plant itself, which is always arbitrarily assigned a zero height when on the ground even if some mosses do aproach 0.1 m tall.




A significant departure from USNVC is an insertion of a number after "H", "S", and "T" to destinguish between:"H1" annuals; "H2" perennials and biennials; "S1" subshrubs and dwarf shrubs; "S2" medium and tall shrubs; "T1" dwarf trees; "T2" medium and tall trees. For Graminiods the "G" is followed by letter indicating whether they are:  "C" cool season (C3); "W" warm season (C4). Warm season (C4) status was only considered from Cyperaceae and Poaceae based on literature references (most genera are one or the other, while Panicum and Cyperus depend on the species). Habit, including physiognomy was determined based on querys of BONAP, tacit knowlege, and sometime literature. Evergreen/deciduous status is uncertain for tropical taxa that only occur outside the US, determinations were made based on distribution and trends in the genus.

}
\usage{
get.habit.code(taxa)
}
\arguments{
\item{taxa}{Plant name as recorded in original plot data.}
}


\examples{
veg <- vegnasis::nasis.veg |> clean.veg()
veg$habitcode <- get.habit.code(veg$taxon)
}
