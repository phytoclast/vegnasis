
#Simplified relationship between basal area and total overstory cover. Derived from field data collected by Grand Rapids Soil Survey Office 2011-2022.
# veg <- clean.veg.log(obs, obsspp)
#
# veg <- veg |> mutate(taxon=harmonize.taxa(veg$taxon, fix=T)) |> fill.type.df() |> fill.nativity.df() |> mutate(symbol = fill.usda.symbols(taxon)) |> fill.hts.df()
# forest <-  veg |> mutate(h = ifelse(diam > 0 & BA > 0, ht.max, NA),
#                          d = ifelse(diam > 0 & BA > 0, diam, NA),
#                          b = ifelse(diam > 0 & BA > 0, BA, NA)) |>
#   group_by(plot) |> filter(ht.max > 5) |>
#   summarise(cover = cover.agg(cover), BA = sum(BA, na.rm = T),
#             h=sum(h*b, na.rm = T), d=sum(d*b, na.rm = T), b=sum(b, na.rm = T)) |> mutate(BA = ifelse(BA > 0, BA, NA),
#                                                                                          ht = ifelse(b == 0, NA, h/b),
#                                                                                          diam = ifelse(b == 0, NA, d/b),
#                                                                                          h = NULL, d = NULL, b=NULL)
# xy <- forest |> subset(!is.na(BA) & !is.na(cover))
# x <- xy$BA
# y <- xy$cover
# mod1 <- minpack.lm::nlsLM(y ~ b1*(1-exp(b2*x))^(b3) , start = list(b1=100, b2=-1, b3=1))#can swap out start values for fixed model values
# summary(mod1)
# >    summary(mod1)
#
# Formula: y ~ b1 * (1 - exp(b2 * x))^(b3)
#
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
#   b1 94.49218    4.16332  22.696  < 2e-16 ***
#   b2 -0.08282    0.02381  -3.478 0.000589 ***
#   b3  0.78939    0.18998   4.155 4.39e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 17.46 on 266 degrees of freedom
#
# Number of iterations to convergence: 11
# Achieved convergence tolerance: 1.49e-08
#2023-03-28 reanalysis with western tree cover seemed to fall short. Probably a lack of commitment in western offices to get the total canopy cover as many excluded plots had cover only in understory, while used basal area for canopy members.



BA.to.cover <- function(x, c=NA){
  df <- data.frame(x=x,c=c)
  b1= 94.49218
  b2= -0.08282
  b3= 0.78939
  df$y = b1*(1-exp(b2*df$x))^b3
  y = ifelse(is.na(df$c), df$y, df$c)
  return(round(y,1))}

# #It was determined that diameter is most closely related to height, and not so much to cover and BA.
# forest <- forest |> mutate(lcover = log(cover), lht=log(ht), ldiam=log(diam), lBA=log(BA), BA2=BA^2, BA.5=BA^.5,
#                            ht2 = ht^2, ht.5 = ht^0.5, diam.5 = diam^0.5, diam2 = diam^2, cover2=cover^2,cover.5=cover^.5)
# #To test bends in the curve, several transformations to see which fit was best.
# cor(forest[,c('diam','ldiam','diam.5','diam2','ht','lht','ht.5','ht2')], use = 'pairwise.complete.obs')
# mod <- lm(ldiam ~ ht.5, forest)
# summary(mod)
# Call:
#   lm(formula = ldiam ~ ht.5, data = forest)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.70323 -0.20670 -0.01468  0.19437  1.12254
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.70799    0.25662   2.759  0.00689 **
#   ht.5         0.61045    0.05264  11.598  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.3275 on 101 degrees of freedom
# (738 observations deleted due to missingness)
# Multiple R-squared:  0.5711,	Adjusted R-squared:  0.5669
# F-statistic: 134.5 on 1 and 101 DF,  p-value: < 2.2e-16
#This function fills in tree diameters scaled with tree heights and to be used in estimating crown widths.
#2023-03-28 --- revision using some larger western tree data... more loglog rather than logsqrt relationship. Diameters do not go up as fast, which makes sense when considering that the Michigan trees reach a ceiling sooner before getting fat compared to west coast trees that keep growing taller.
# veg <- clean.veg.log(obs, obsspp)
# veg.raw2 <- readRDS('data/veg.raw.select.RDS')
# veg2 <- clean.veg(veg.raw2)
# veg <- rbind(veg, veg2)
# > forest <- forest |> mutate(lcover = log(cover), lht=log(ht), ldiam=log(diam), lBA=log(BA), BA2=BA^2, BA.5=BA^.5,
#                              +                            ht2 = ht^2, ht.5 = ht^0.5, diam.5 = diam^0.5, diam2 = diam^2, cover2=cover^2,cover.5=cover^.5)
# > #To test bends in the curve, several transformations to see which fit was best.
#   > cor(forest[,c('diam','ldiam','diam.5','diam2','ht','lht','ht.5','ht2')], use = 'pairwise.complete.obs')
# diam     ldiam    diam.5     diam2        ht       lht      ht.5       ht2
# diam   1.0000000 0.9456846 0.9869121 0.9592726 0.6983876 0.6975235 0.7088525 0.6386003
# ldiam  0.9456846 1.0000000 0.9853416 0.8239491 0.7365404 0.7969686 0.7767658 0.6303564
# diam.5 0.9869121 0.9853416 1.0000000 0.9034591 0.7287848 0.7555622 0.7529961 0.6462417
# diam2  0.9592726 0.8239491 0.9034591 1.0000000 0.5919818 0.5585332 0.5847551 0.5675288
# ht     0.6983876 0.7365404 0.7287848 0.5919818 1.0000000 0.9498357 0.9884026 0.9674704
# lht    0.6975235 0.7969686 0.7555622 0.5585332 0.9498357 1.0000000 0.9860509 0.8456052
# ht.5   0.7088525 0.7767658 0.7529961 0.5847551 0.9884026 0.9860509 1.0000000 0.9193550
# ht2    0.6386003 0.6303564 0.6462417 0.5675288 0.9674704 0.8456052 0.9193550 1.0000000
# > mod <- lm(ldiam ~ lht, forest)
# > summary(mod)
#
# Call:
#   lm(formula = ldiam ~ lht, data = forest)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.73190 -0.19268 -0.00793  0.15818  1.14965
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.04577    0.21177   0.216    0.829
# lht          1.14555    0.06582  17.405   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.2901 on 174 degrees of freedom
# (1153 observations deleted due to missingness)
# Multiple R-squared:  0.6352,	Adjusted R-squared:  0.6331
# F-statistic: 302.9 on 1 and 174 DF,  p-value: < 2.2e-16

#Function fills in missing diameters using a vector of heights. Intended to provide reasonable estimates for graphing tree shapes.
#REVISION 20230402 Downloaded Treedb from Native Tree Society
# treedb <- read.csv('data/treedb.csv') |> subset(dbh < 900 & ht.max > 5 & dbh > 10) #take out outlyers.
#
# forest <- treedb |> mutate(ht=ht.max, lht=log(ht), ldiam=log(dbh),
#                            ht2 = ht^2, ht.5 = ht^0.5, diam.5 = dbh^0.5, diam2 = dbh^2, lcw = log(cw)) |> subset(!is.na(ht) & !is.na(dbh))
# #To test bends in the curve, several transformations to see which fit was best.
# cor(forest[,c('dbh','ldiam','diam.5','diam2','ht','lht','ht.5','ht2','cw','lcw')], use = 'pairwise.complete.obs')
#
# mod <- lm(log(dbh)~log(ht.max)+0, data=forest)
# summary(mod)
# Call:
#   lm(formula = log(dbh) ~ log(ht.max) + 0, data = forest)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -1.77038 -0.28567 -0.04015  0.26151  3.01887
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# log(ht.max) 1.248705   0.000975    1281   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.4443 on 17494 degrees of freedom
# Multiple R-squared:  0.9894,	Adjusted R-squared:  0.9894
# F-statistic: 1.64e+06 on 1 and 17494 DF,  p-value: < 2.2e-16

fill.diameters <- function(x, dbh.max=NA,dbh.min=NA){
  d <- ifelse(is.na(dbh.min), dbh.max, (dbh.max+dbh.min)/2)
  df <- data.frame(x=x,d=d)
  df$y = ifelse(x > 5, exp(1.248705*log(df$x)), NA_real_)
  y = round(ifelse(is.na(df$d), df$y,df$d),1)
  return(y)}

#Function estimates number of trees per hectare based on stem diameter and basal area.
#b=basal area m2 per ha.
#d=diameter cm.
trees_per_ha <- function(b,d){
  t = b/((d/200)^2*3.141592)
  return(t)
}

BA_per_ha <- function(dens,dbh){
  BA = round(dens*(dbh/200)^2*3.141592,1)
  return(BA)
}

# Estimated crown width based on density in stems per ha, and total cover for that taxon/stratum.
# mod1 <- minpack.lm::nlsLM(cw ~ b1*(1-exp(b2*dbh))^(b3), data=forest , start = list(b1=100, b2=-1, b3=1))
# summary(mod1)
# Formula: cw ~ b1 * (1 - exp(b2 * dbh))^(b3)
#
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
# b1 69.690000  22.148667   3.146  0.00169 **
#   b2 -0.002392   0.001376  -1.738  0.08245 .
# b3  0.857591   0.078129  10.977  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6.032 on 1162 degrees of freedom
#
# Number of iterations to convergence: 42
# Achieved convergence tolerance: 1.49e-08

est_crown_width <- function(dbh){
  b1= 69.690000
  b2= -0.002392
  b3=  0.857591
  crown.width = round(b1*(1-exp(b2*dbh))^(b3),1)
  return(crown.width)
}

#old formula
# est_crown_width <- function(density, cover, diam){
#   cf = 0.8 #correction factor to average between two cover aggregate methods. More research needed.
#   crownarea <- (1-(1-cover/100)^(1/density))*10000*(1-cf) + cover/100*10000/density*(cf)
#   c.w <- 2*(crownarea/3.141592)^0.5
#   crown.width <- round(pmin(pmax(pmin(0.50*diam,pmax(0.02*diam, c.w)),1),30),1)#constrain crown width to between 2 and 50 times stem diameter and 1 to 30 m.
#   return(crown.width)
# }

#function estimates (poorly) the required number of individuals of given crown width for a target canopy cover.
density_from_cw <- function(cover,cw){
  cf=0.8
  c =cover
  w = (cw/2)^2*3.141592
  n1 <- log(1-c/100)/log(1-w/10000)
  n2 <- c/(100*(w/10000))
  n3 <- n1*(1-cf)+n2*cf
  return(round(n3,0))
}


