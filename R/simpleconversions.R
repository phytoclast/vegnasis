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
#

BA.to.cover <- function(x){
  b1= 94.49218
  b2= -0.08282
  b3= 0.78939
  y = b1*(1-exp(b2*x))^b3
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
fill.diameters <- function(x){
  y = exp(0.70799 + 0.61045*h^0.5)
  return(y)}


