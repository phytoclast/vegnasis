# veg <- clean.veg.log(obs, obsspp)
# badrecords <- veg |> subset(!is.na(BA) & is.na(diam))
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
# df <- forest |> subset(!is.na(BA) & !is.na(cover) & !is.na(diam) &  cover >= 75 & !plot %in% badrecords$plot)
# df <- df |> mutate(dens = BA/(3.141592*(diam/200)^2), crwd = 2*((1-(1-cover/100)^(1/dens))*10000/3.141592)^0.5, 
#                    confinedcrwd = ((10000/dens)/3.141592)^0.5*2) 
# x <- df$diam
# y <- df$crwd
# z <- df$confinedcrwd
# mod1 <- minpack.lm::nlsLM(z ~ b1*(1-exp(b2*x))^(b3) , start = list(b1=30, b2=-1, b3=1))
# summary(mod1)
# 
# Formula: z ~ b1 * (1 - exp(b2 * x))^(b3)
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)  
# b1 40.654624  63.037151   0.645   0.5224  
# b2 -0.009636   0.018983  -0.508   0.6143  
# b3  1.442356   0.681484   2.116   0.0401 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.555 on 43 degrees of freedom
# 
# Number of iterations till stop: 50 
# Achieved convergence tolerance: 1.49e-08
# Reason stopped: Number of iterations has reached `maxiter' == 50.
# 

make.crown.width <- function(diam){
b1 = 40.654624    
b2 = -0.009636      
b3 = 1.442356      
crwd <- b1*(1-exp(b2*diam))^(b3)
return(crwd)
}


# dfba <-  forest |> subset(!is.na(BA) & !is.na(cover))
# x=dfba$cover 
# y=dfba$BA 
# 
# 
# mod1  <- lm(y ~ x)
# summary(mod1)
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -21.366  -4.813  -0.376   4.105  48.397 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.93199    1.94523   1.507    0.133    
# x            0.25651    0.02396  10.706   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.967 on 267 degrees of freedom
# Multiple R-squared:  0.3004,	Adjusted R-squared:  0.2977 
# F-statistic: 114.6 on 1 and 267 DF,  p-value: < 2.2e-16


make.BA <- function(cover){
BA <- 2.93199+0.25651*cover
  return(BA)
}
