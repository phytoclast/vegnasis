# require(ggplot2)
# require(ggpp)
# ntrees <-  function(totalcover,crown.width, transect.length){
#   numberoftrees = log(1-totalcover)/log(1-crown.width/transect.length)
#   return(numberoftrees)}
#
# make.tree <- function(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent){
#   #form.choice='E'
#   forms = c('E','D','Ds','B','Bs','N','G','F1','F2','EX','Dx','Dsx','GX','FX')
#   f.color = c('darkgreen','green','green','darkgreen','darkgreen','yellow','yellow','green','darkgreen','gray','gray','gray','gray','gray')
#   use.color = f.color[which(forms == form.choice)]
#   scxp <- scale.exponent#scxp=1
#   tree.ht=tree.height/tree.height
#   crn.ht=branch.height/tree.height
#   crn.wd=crown.width/tree.height
#   tree.wd=diameter/100/tree.height
#
#
#   g.E.crown <- polygonGrob(x=c(0.5-crn.wd/2, 0.5, 0.5+crn.wd/2),
#                            y=c(crn.ht, tree.ht, crn.ht)^scxp, gp=gpar(fill=use.color,alpha=0.5))
#
#   g.D.crown <- polygonGrob(x=c(0.5-crn.wd/2,
#                                0.5-crn.wd/2,
#                                0.5-0.75*crn.wd/2,
#                                0.5-0.5*crn.wd/2,
#                                0.5-0.25*crn.wd/2,
#                                0.5,
#                                0.5+0.25*crn.wd/2,
#                                0.5+0.5*crn.wd/2,
#                                0.5+0.75*crn.wd/2,
#                                0.5+crn.wd/2,
#                                0.5+crn.wd/2),
#                            y=c(crn.ht,
#                                (crn.ht*0.33+tree.ht*0.67),
#                                (crn.ht*0.15+tree.ht*0.85),
#                                (crn.ht*0.05+tree.ht*0.95),
#                                (crn.ht*0.02+tree.ht*0.98),
#                                tree.ht,
#                                (crn.ht*0.02+tree.ht*0.98),
#                                (crn.ht*0.05+tree.ht*0.95),
#                                (crn.ht*0.15+tree.ht*0.85),
#                                (crn.ht*0.33+tree.ht*0.67),
#                                crn.ht)^scxp, gp=gpar(fill=use.color,alpha=0.5))
#
#   g.Ds.crown <- polygonGrob(x=c(-1,
#                                 -0.9,
#                                 -0.75,
#                                 -0.5,
#                                 -0.25,
#                                 0,
#                                 0.25,
#                                 0.5,
#                                 0.75,
#                                 0.9,
#                                 1)/2+0.5,
#                             y=(c(0,
#                                  0.5,
#                                  0.7,
#                                  0.9,
#                                  0.95,
#                                  1.00,
#                                  0.95,
#                                  0.9,
#                                  0.7,
#                                  0.5,
#                                  0)*.67+0.33)^scxp, gp=gpar(fill=use.color,alpha=0.5))
#
#
#
#   g.trunk <- rectGrob(x=0.5, y=((crn.ht-0)^scxp)/2, width=tree.wd, height=(crn.ht-0)^scxp, gp=gpar(fill='brown'))
#
#   g.stems <- polygonGrob(x=c(-0.075,-0.13,-0.12,-0.025,-0.005,0.005,0.025,0.12,0.13,0.075)+0.5,
#                          y=(c(0,1,1,0,1,1,0,1,1,0)*0.33)^scxp, gp=gpar(fill='brown'))
#
#
#   g.G.crown <- polygonGrob(x=c(0.425,0.375,0.475,0.5,0.525,0.625,0.575),
#                            y=c(0,0.85,0,1,0,0.85,0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
#
#   g.F1.crown <- polygonGrob(x=c(0.46, 0.46, 0.18, 0.12, 0.05, 0, 0.05, 0.12, 0.18, 0.46, 0.46, 0.37, 0.38, 0.5, 0.6, 0.62, 0.53, 0.54, 0.79, 0.84, 0.94, 1, 0.93, 0.84, 0.8, 0.54, 0.55, 0.46),
#                             y=c(0, 0.39, 0.57, 0.52, 0.52, 0.61, 0.7, 0.7, 0.61, 0.45, 0.78, 0.82, 0.95, 1, 0.95, 0.82, 0.78, 0.45, 0.68, 0.76, 0.77, 0.68, 0.59, 0.59, 0.63, 0.39, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
#   g.F2.crown <- polygonGrob(x=c(0.41, 0.38, 0.34, 0.28, 0.19, 0, 0.22, 0.38, 0.35, 0.4, 0.44, 0.44, 0.37, 0.45, 0.49, 0.56, 0.52, 0.5, 0.53, 0.45, 0.46, 0.52, 0.6, 0.59, 0.74, 1, 0.77, 0.67, 0.61, 0.53, 0.47, 0.41),
#                             y=c(0, 0.13, 0.2, 0.1, 0.29, 0.41, 0.43, 0.32, 0.24, 0.15, 0.02, 0.38, 0.44, 0.88, 0.96, 1, 0.94, 0.85, 0.42, 0.38, 0.03, 0.16, 0.28, 0.4, 0.45, 0.44, 0.27, 0.12, 0.25, 0.13, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
#
#   if(form.choice %in% c('E','N')){
#     g.tree = grobTree(g.E.crown, g.trunk)
#   }
#   if(form.choice %in% c('D','B')){
#     g.tree = grobTree(g.D.crown, g.trunk)
#   }
#   if(form.choice %in% c('Ds','Bs')){
#     g.tree = grobTree(g.Ds.crown, g.stems)
#   }
#   if(form.choice %in% c('G','GX')){
#     g.tree = grobTree(g.G.crown)
#   }
#   if(form.choice %in% c('F1','FX')){
#     g.tree = grobTree(g.F1.crown)
#   }
#   if(form.choice %in% c('F2')){
#     g.tree = grobTree(g.F2.crown)
#   }
#
#   return(g.tree)
#
# }
#
# make.stratum <- function(veg.grob, tree.height, branch.height, crown.width, diameter, scale.exponent){
#   n=round(ntrees(total.cover, crown.width, 50),0)
#   x = sample(0:200, n, replace = T)/4
#   rando =  rnorm(n)
#   height = rando*0.05*tree.height+tree.height-0.05/2*tree.height
#   width = rando*0.05*crown.width+crown.width
#   y = height
#
#   grobs.tb <- tibble(x = x, y = (y^scale.exponent)/2,
#                      width = width/50,
#                      height =  (height/50)^scale.exponent,
#                      grob = list(veg.grob))
#   return(grobs.tb)
# }
#
# #Establish crown width scaled to diameter
# get.crown.diam.ratio <- function(cover, dbh, ba){
#   #cover = aggregate overstory cover
#   #dbh = stand quadratic mean diameter
#   #ba = stand basal area count
#
#   m2pertree = 3.141592*(dbh/200)^2
#   treesperha = ba/m2pertree
#   coverpertreeperha = 1-(1-cover)^(1/treesperha)
#   crown.width = 2*(coverpertreeperha*10000/3.141592)^0.5
#   crown.stem.ratio = crown.width/dbh*100
#   return(crown.stem.ratio)#crown stem ratio meter per meter
# }
#
# tree.height=45
# branch.height=25
# crown.width=20
# diameter=120
# form.choice = 'E'
#
# total.cover = 0.7
# assemble.strata <- function()
# tree0 <- make.tree(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent)
#
# stratum0 <- make.stratum(tree0, tree.height, branch.height, crown.width, diameter, scale.exponent)
#
# all.strata <- rbind(stratum0,stratum1,stratum2,stratum3,stratum4)
#
# all.strata <- all.strata[sample(1:nrow(all.strata),133),]
#
#
# if(scale.exponent <=0.7){
#   labels1=c(0:4,(1:25)*5)
#   breaks1=(c(0:4,(1:25)*5))^scale.exponent
# }else{
#   labels1=c(1:25)*5
#   breaks1=(c(1:25)*5)^scale.exponent
# }
#
# ggplot() +
#   geom_grob(data = all.strata,
#             aes(x, y, label = grob, vp.width = width, vp.height = height)
#   ) +
#   scale_y_continuous(name='canopy height (m)',breaks=breaks1,labels=labels1) +
#   scale_x_continuous(breaks=c(0:25)*5) +
#   coord_fixed(ratio = 50/50^scale.exponent, ylim = c(0,50^scale.exponent), xlim = c(0,50), expand = F)+
#   theme_bw(12)
#
#
#
#
