#This function prepares strata for plotting.
prepare_strata <- function(veg){
veg <- veg  |> fill.type.df() |> fill.hts.df()

veg <- veg |> mutate(dbh.r =  fill.diameters(ht.max,dbh.max,dbh.min))
veg <- veg |> mutate(cw =  case_when(type %in% 'tree' | ht.max > 5 ~ pmax(est_crown_width(dbh.r),1),
                                     type %in% 'shrub/vine' ~ pmax(pmin(3,ht.max),1),
                                     TRUE ~ 1))
veg <- veg |> mutate(density =  density_from_cw(cover, cw))
veg <- veg |> mutate(BA.r =  BA_per_ha(density, dbh.r))

veg <- veg |> group_by(plot) |> mutate(BA.sum = sum(BA, na.rm = T), BA.rsum = sum(BA.r, na.rm = T), BA.sum = ifelse(is.na(BA.sum), BA.rsum,BA.sum), BA.ratio = BA.sum/BA.rsum,  BA.rsum = NULL)#

veg <- veg |> mutate(BA.r = ifelse(ht.max <= 5, BA.r, round(BA.r*BA.ratio,1)),
                     density = ifelse(ht.max <= 5, density, round(density*BA.ratio,0)),
                     cw = ifelse(ht.max <= 5, cw, round(cw*BA.ratio^-0.5,1)))

veg <- veg |> mutate(habit= get.habit.code(taxon),
                     crshape = case_when(grepl('^T', habit) & grepl('N', habit) ~ 'conifer1',
                                         grepl('^T', habit)  ~ 'blob',
                                         type %in% 'shrub/vine' ~ 'cloud1',
                                         grepl('FE', habit) ~ 'ferny',
                                         grepl('F', habit) ~ 'forby',
                                         type %in% 'grass/grasslike' ~ 'grassy'),
                     stshape = case_when(grepl('^T', habit) & grepl('N', habit) ~ 'trunk',
                                         grepl('^T', habit)  ~ 'trunk',
                                         type %in% 'shrub/vine' ~ 'sticks',
                                         grepl('FE', habit) ~ NA,
                                         grepl('F', habit) ~ NA,
                                         type %in% 'grass/grasslike' ~ NA),
                     fun=case_when(grepl('^T', habit)  ~ 'T',
                                   grepl('^S', habit)  ~ 'S',
                                   grepl('^H', habit)  ~ 'H'),
                     stems = round(0.1*density,0) #count number of stem objects required for tenth hectare plot
)
strats <- veg |> subset(fun %in% c('T','S','H') & stems > 0) |> arrange(plot,-ht.max, -cover)#reduce to strata which have models and are not empty of stems
strats$seq <- c(1:nrow(strats))
strats <- strats |> group_by(plot) |> mutate(seqmin = min(seq), seq = seq-seqmin+1, seqmin = NULL)


#Establish a stand 50 by 20 m.
stand <- make_hex_stand(0.5,1) |> subset(yp >= 15 & yp < 35) |> mutate(wtn = wt, stratid = NA)

#assign stump locations per stratum
for (i in 1:nrow(strats)){#i=1
  thistrat = strats$seq[i]
  nstems = strats$stems[i]
  newstumps <- sample(stand$stumpid, size = nstems, prob = stand$wtn, replace = T)
  stand <- stand |> mutate(wtn = ifelse(stand$stumpid %in% newstumps, 0.0001, wtn),
                           stratid = ifelse(stand$stumpid %in% newstumps, thistrat,stratid))
}

#Create shapes of the right size, then distribute into the stump positions.
for (i in 1:nrow(strats)){#i=1
  thistrat <- strats[i,]
  plant0 <- make_plant(thistrat$fun, thistrat$ht.max, thistrat$ht.min,thistrat$cw,thistrat$dbh.r, thistrat$crshape, thistrat$stshape)
  stumps0 <- stand |> subset(stratid %in% i)
  plant0 <- merge(stumps0, plant0) |> mutate(objid = paste0(stratid,obj,stumpid))
  if(i==1){plants <- plant0}else{plants <- rbind(plants,plant0)}
}

#randomize sizes and positions
plants <- plants |> group_by(stumpid) |>
  mutate(ht.max = max(z), crwd = max(x)-min(x),
         xpp = xp + runif(1, min = -0.8, max = 0.8),#shift position on grid
         zr = rnorm(1,ht.max, ht.max/10)/ht.max,#deviation in height
         xr = (rnorm(1,ht.max, ht.max/10)/ht.max+rnorm(1,crwd, crwd/10)/crwd)/2,#deviation in width partially related to height
         xn = x*xr+xpp,#resized width and put on new position
         zn = z*zr*(1-1/15))#resized height adjusted downward show that variation is less than max height in the field

return(plants)
}
