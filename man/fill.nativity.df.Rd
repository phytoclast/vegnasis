\name{fill.nativity.df}
\alias{fill.nativity.df}
\title{Fill missing nativity status on a cleaned vegetation data frame}
\description{
This function fills in missing nativity data based taxon status across broad regions of North America. Taxon may still be introduced (or adventive) within a state in that region, but native considering the area collectively. If a region is not specified, the default is the nativity across the United States. This version of the function take a vegetation plot data frame (cleaned by the clean.veg function) as input.

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
fill.nativity.df(df,  region=NA)
}
\arguments{
\item{df}{Dataframe containing appropriately taxon and nativity columns.}
\item{region}{Region of North America.}
}


\examples{
veg.raw <- vegnasis::nasis.veg
veg <- clean.veg(veg.raw)
veg$nativity <- NA #remove existing values if new values are need. Otherwise, existing values will be preserved and only missing values will be filled.
veg <- fill.nativity.df(veg, region="Northeast"))
# Altenative using pipes.
veg <- veg |> mutate(fill.nativity.df(region="Northeast"))
}
