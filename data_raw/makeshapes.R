angles <- c(0:59)/60*2*3.141592
#coniferoverhead
wave = ((sin(angles*9)+1)/2)^1
x = cos(angles)*(wave+1)/2; y=sin(angles)*(wave+1)/2
shape='conifer'
conifer <- data.frame(shape=shape, x=x,y=y)
#hardwoodoverhead
wave = ((sin(angles*9)+1)/2)^0.5
x = cos(angles)*(wave+4)/5; y=sin(angles)*(wave+4)/5
shape='hardwood'
hardwood <- data.frame(shape=shape, x=x,y=y)
#circle
x = cos(angles); y=sin(angles)
shape='circle'
circle <- data.frame(shape=shape, x=x,y=y)
overheadshapes <- rbind(circle, hardwood, conifer)
overheadshapes <- overheadshapes |> mutate(x=x/2, y=y/2)
ggplot(overheadshapes, aes(x=x, y=y, color=shape))+
  geom_polygon(fill='#FFFFFF00')
write.csv(overheadshapes, 'C:/scripts/vegnasis/data_raw/overheadshapes.csv', row.names = F)
usethis::use_data(overheadshapes, overwrite = T)
