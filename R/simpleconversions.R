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


