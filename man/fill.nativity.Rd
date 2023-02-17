\name{fill.nativity}
\alias{fill.nativity}
\title{Fill missing nativity status}
\description{
This function fills in missing nativity data based taxon status across broad regions of North America. Taxon may still be introduced (or adventive) within a state in that region, but native considering the area collectively.

##Regions:

'Northwest' = OR, ID, MT, WA, WY.

'Southwest' = AZ, CA, CO, NM, NV, UT.

'NorthCentral' = IA, MO, MN, ND, NE, SD.

'Southcentral' = AR, LA, KS, OK, TX.

'Northeast' = CT, DE, DC, IL, IN, KY, MA, MD, ME, MI, NH, NJ, NY, OH, PA, RI, WI, VT, VA, WV.

'Southeast' = AL, GA, FL, MS, NC, SC, TN.

'Alaska' = Alaska

'Hawaii' = Hawaii

'Caribbean' = Puerto Rico, Virgin Islands.

'CanadaWest' =  Alberta, British Columbia, Manitoba, Saskatchewan.

'CanadaEast' =  Labrador, New Brunswick, Newfoundland, Prince Edward Island, Quebec, Nova Scotia, Ontario.

'Arctic' = Northwest Territories, Nunavut, Yukon, Greenland.

'Mexico' = All of Mexico


}
\usage{
fill.nativity(taxa,  region=NA, nativity=NA)
}
\arguments{
\item{taxa}{Taxon names.}
\item{region}{Region of North America.}
\item{nativity}{Existing nativity status (optional) to ensure that existing statuses get priority over look up values.}
}


\examples{
veg.raw <- vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
veg$nativity <- NA #To remove existing values if new values are needed. Otherwise, existing values will be preserved and only missing values will be filled.
veg$nativity <- fill.nativity(taxa=veg$taxon, region="Northeast", nativity = veg$nativity)
# Altenative using pipes.
veg <- veg |> mutate(nativity = fill.nativity(taxa=taxon, region="Northeast", nativity = nativity))
}
