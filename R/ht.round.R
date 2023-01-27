#This function rounds heights to the nearest meter for plants taller than 8 meters, to the nearest tenth meter for plants less than 3 meters, and to the nearest half meter for intermediate heights.
ht.round <- function(ht){
  ifelse(ht >= 8,round(ht,0),
         ifelse(ht >= 3, floor(ht*2+0.499)/2,round(ht,1)
         ))}
