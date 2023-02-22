library(dplyr)

#This function rounds heights to the nearest meter for plants taller than 8 meters, to the nearest tenth meter for plants less than 3 meters, and to the nearest half meter for intermediate heights.
ht.round <- function(ht){
  ifelse(ht >= 8,round(ht,0),
         ifelse(ht >= 3, floor(ht*2+0.499)/2,round(ht,1)
         ))}

#This function converts plant height from NASIS to meters.

ht.metric <- function(ft){
  round(as.numeric(ft)*0.3048,1)
}

#This function converts plant heights from meters to feet.

ht.medieval <- function(m){
  round(as.numeric(m)/0.3048,1)
}

#This function converts tree diameters from NASIS to centimeters.

diam.metric <- function(inch){
  round(as.numeric(inch)*2.54,0)
}

#This function converts tree diameters from centimeters to inches.

diam.medieval <- function(cm){
  round(as.numeric(cm)/2.54,1)
}
#For taking raw tree count and prism factor and calculating basal area in square meters per hectare. Basal area factors less than 5 (e.g. baf=2) are assumed to be SI, while factors 5 and above are assumed USC.
tree.ct.BA <- function(x, baf=10) {
  case_when(baf < 5 ~ x * baf,
            TRUE ~ round(x * baf*10000/43560,1))}
#Tree basal area unit conversion from square meters per hectare to square feet per acre.
BA.to.USC<-function(ba){
  round(ba*43560/10000,1)}
#Tree basal area unit conversion from square feet per acre to square meters per hectare.
BA.to.SI<-function(ba){
  round(ba*10000/43560,1)}
