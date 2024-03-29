#convert svg ----
library(svgparser)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

convert.SVG <- function(filename){

  blob <- svgparser::read_svg(filename, obj_type = 'data.frame')

  blob$x <- (blob$x - (max(blob$x) + min(blob$x))/2) / (max(blob$x) - min(blob$x))
  blob$y <- 1-1*(blob$y - min(blob$y)) / (max(blob$y) - min(blob$y))
  blob <- subset(blob, select=c(x,y))
  return(blob)

}
branch1 <- convert.SVG('branch1.svg')
blob <- convert.SVG('blob.svg')
trunk <- convert.SVG('trunk.svg')
palm <- convert.SVG('palm.svg')

write.csv(branch1, 'branch1.csv', row.names = F)
write.csv(blob, 'blob.csv', row.names = F)
write.csv(trunk, 'trunk.csv', row.names = F)
write.csv(palm, 'palm.csv', row.names = F)

#convert to template ----



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

branch1 <- read.csv('branch1.csv') |> data.frame(name='branch1')
conifer1 <- read.csv('conifer1.csv') |> data.frame(name='conifer1')
conifer3 <- read.csv('conifer1.csv') |> data.frame(name='conifer3')
conifer2 <- read.csv('conifer2.csv') |> data.frame(name='conifer2')
cloud1 <- read.csv('cloud1.csv') |> data.frame(name='cloud1')
flattop <- read.csv('flattop.csv') |> data.frame(name='flattop')
flattop2 <- read.csv('flattop2.csv') |> data.frame(name='flattop2')
blob1 <- read.csv('blob.csv') |> data.frame(name='blob1')
blob2 <- read.csv('blob.csv') |> data.frame(name='blob2')
palm <- read.csv('palm.csv') |> data.frame(name='palm')|> mutate(x=round(x,2),y=round(y,2))
trunk <- read.csv('trunk.csv') |> data.frame(name='trunk')|> mutate(x=round(x,2),y=round(y,2))
triangle <- data.frame(x=c(-0.5, 0, 0.5),y=c(0, 1, 0), name='triangle')
dome <- data.frame(x=c(-1,
                       -0.9,
                       -0.75,
                       -0.5,
                       -0.25,
                       0,
                       0.25,
                       0.5,
                       0.75,
                       0.9,
                       1)/2,
                   y=(c(0,
                        0.5,
                        0.7,
                        0.9,
                        0.95,
                        1.00,
                        0.95,
                        0.9,
                        0.7,
                        0.5,
                        0)), name='dome')
sticks <- data.frame(x=c(-0.075,-0.13,-0.12,-0.025,-0.005,0.005,0.025,0.12,0.13,0.075),y=c(0,1,1,0,1,1,0,1,1,0), name='sticks')


grassy <- data.frame(x=c(-0.3, -0.5, -0.1,  0.0,  0.1,  0.5,  0.3), y=c(0,0.85,0,1,0,0.85,0), name='grassy')


forby <- data.frame(x=c(-0.04, -0.04, -0.32, -0.38, -0.45, -0.50, -0.45, -0.38, -0.32, -0.04, -0.04, -0.13, -0.12,  0.00,  0.10,  0.12,  0.03,  0.04,  0.29,  0.34,  0.44,  0.50,  0.43,  0.34,  0.30, 0.04, 0.05, -0.04),
                    y=c(0, 0.39, 0.57, 0.52, 0.52, 0.61, 0.7, 0.7, 0.61, 0.45, 0.78, 0.82, 0.95, 1, 0.95, 0.82, 0.78, 0.45, 0.68, 0.76, 0.77, 0.68, 0.59, 0.59, 0.63, 0.39, 0, 0), name='forby')
ferny <- data.frame(x=c(-0.09, -0.12, -0.16, -0.22, -0.31, -0.50, -0.28, -0.12, -0.15, -0.10, -0.06, -0.06, -0.13, -0.05, -0.01,  0.06,  0.02,  0.00,  0.03, -0.05, -0.04,  0.02,  0.10,  0.09,  0.24, 0.50,  0.27,  0.17,  0.11,  0.03, -0.03, -0.09),
                    y=c(0, 0.13, 0.2, 0.1, 0.29, 0.41, 0.43, 0.32, 0.24, 0.15, 0.02, 0.38, 0.44, 0.88, 0.96, 1, 0.94, 0.85, 0.42, 0.38, 0.03, 0.16, 0.28, 0.4, 0.45, 0.44, 0.27, 0.12, 0.25, 0.13, 0, 0), name='ferny')

shapes <- rbind(branch1,conifer1,conifer2,conifer3,cloud1,flattop,flattop2,blob1,blob2,trunk,triangle,dome,sticks,grassy,forby,ferny,palm)
colnames(shapes) <- c("x","z","shape","fill","color")
saveRDS(shapes,'shapes.RDS')
shapes <- readRDS('shapes.RDS')
usethis::use_data(shapes, overwrite = T)



