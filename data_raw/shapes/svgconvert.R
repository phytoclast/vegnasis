setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# remotes::install_github('https://github.com/pmur002/gggrid')
# install.package('remotes')
# remotes::install_github('coolbutuseless/cssparser') # Handles CSS styling
# remotes::install_github('coolbutuseless/svgparser')
library(svgparser)
#https://cran.r-project.org/web/packages/ggpp/vignettes/grammar-extensions.html#geom_grob



convert.SVG <- function(filename){

  blob <- svgparser::read_svg(filename, obj_type = 'data.frame')

  blob$x <- (blob$x - (max(blob$x) + min(blob$x))/2) / (max(blob$x) - min(blob$x))
  blob$y <- 1-1*(blob$y - min(blob$y)) / (max(blob$y) - min(blob$y))
  blob <- subset(blob, select=c(x,y))
  return(blob)

}

blob <- convert.SVG('blob.svg')
trunk <- convert.SVG('trunk.svg')
palm <- convert.SVG('palm.svg')

write.csv(blob, 'blob.csv', row.names = F)
write.csv(trunk, 'trunk.csv', row.names = F)
write.csv(palm, 'palm.csv', row.names = F)

conifer1 <- convert.SVG('conifer1.svg') 
write.csv(conifer1, 'conifer1.csv', row.names = F)

conifer2 <- convert.SVG('conifer2.svg') 
write.csv(conifer2, 'conifer2.csv', row.names = F)

cloud1 <- convert.SVG('cloud1.svg') 
write.csv(cloud1, 'cloud1.csv', row.names = F)

flattop <- convert.SVG('flattop.svg') 
write.csv(flattop, 'flattop.csv', row.names = F)

flattop2 <- convert.SVG('flattop2.svg') 
write.csv(flattop2, 'flattop2.csv', row.names = F)