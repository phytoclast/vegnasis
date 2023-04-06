#This function prepares strata for plotting.
grow_plants <- function(veg, plength = 50, pwidth=20){
  veg <- veg  |> fill.type.df() |> fill.hts.df()
  #check to see if habit code exists, then only fill in missing values---
  if(!'habit'%in% colnames(veg)){veg <- veg |> mutate(habit = get.habit.code(taxon))
  }else{#override default values
    veg <- veg |> mutate(prehabit =get.habit.code(taxon),
                         habit = ifelse(is.na(veg$habit),prehabit,habit),
                         prehabit=NULL)}
  veg <- veg |> mutate(dbh.r =  fill.diameters(ht.max,dbh.max,dbh.min))
  veg <- veg |> mutate(cw =  case_when(grepl('^T', habit) | ht.max > 5 ~ pmax(est_crown_width(dbh.r),1),
                                       grepl('^S', habit) ~ pmax(pmin(3,ht.max),1),
                                       TRUE ~ 1))
  veg <- veg |> mutate(density =  density_from_cw(cover, cw))
  veg <- veg |> mutate(BA.r =  BA_per_ha(density, dbh.r))

  veg <- veg |> group_by(plot) |> mutate(BA.sum = sum(BA, na.rm = T), BA.rsum = sum(BA.r, na.rm = T), BA.sum = ifelse(is.na(BA.sum) | BA.sum <=0, BA.rsum,BA.sum), BA.ratio = BA.sum/BA.rsum,  BA.rsum = NULL)#

  veg <- veg |> mutate(BA.r = ifelse(ht.max <= 5, BA.r, round(BA.r*BA.ratio,1)),
                       density = ifelse(ht.max <= 5, density, round(density*BA.ratio,0)),
                       cw = ifelse(ht.max <= 5, cw, round(cw*BA.ratio^-0.5,1)))

  veg <- veg |> mutate(crshape0 = case_when(grepl('^T', habit) & grepl('NE', habit) ~ 'conifer1',
                                           grepl('^T', habit) & grepl('N', habit) ~ 'conifer3',
                                           grepl('^T', habit) & grepl('P', habit) ~ 'palm',
                                           grepl('^T', habit) & grepl('F', habit) ~ 'palm',
                                           grepl('^T', habit) & grepl('BE', habit) ~ 'blob2',
                                           grepl('^T', habit)  ~ 'blob1',
                                           grepl('^S', habit) & grepl('P', habit) ~ 'palm',
                                           grepl('^S', habit) & grepl('F', habit) ~ 'palm',
                                           grepl('^S', habit) & grepl('BE', habit) ~ 'blob2',
                                           grepl('^S', habit) ~ 'cloud1',
                                           grepl('FE', habit) ~ 'ferny',
                                           grepl('^H', habit) & grepl('F', habit) ~ 'forby',
                                           grepl('^H', habit) & type %in% 'grass/grasslike' ~ 'grassy'),
                       crfill0 = case_when(grepl('^T', habit) & grepl('NE', habit) ~ '#1A801A',
                                           grepl('^T', habit) & grepl('N', habit) ~ 'green',
                                           grepl('^T', habit) & grepl('P', habit) ~ 'green',
                                           grepl('^T', habit) & grepl('F', habit) ~ 'green',
                                           grepl('^T', habit) & grepl('BE', habit) ~ '#1A801A',
                                           grepl('^T', habit)  ~ '#4DE600',
                                           grepl('^S', habit) & grepl('P', habit) ~ 'green',
                                           grepl('^S', habit) & grepl('F', habit) ~ 'green',
                                           grepl('^S', habit) & grepl('BE', habit) ~ '#1A801A',
                                           grepl('^S', habit) ~ 'green',
                                           grepl('FE', habit) ~ 'green',
                                           grepl('^H', habit) & grepl('F', habit) ~ 'magenta',
                                           grepl('^H', habit) & type %in% 'grass/grasslike' ~ 'yellowgreen'),
                       crcolor0 = case_when(grepl('^T', habit) & grepl('NE', habit) ~ 'darkgreen',
                                            grepl('^T', habit) & grepl('N', habit) ~ 'darkgreen',
                                            grepl('^T', habit) & grepl('P', habit) ~ 'darkgreen',
                                            grepl('^T', habit) & grepl('F', habit) ~ 'darkgreen',
                                            grepl('^T', habit) & grepl('BE', habit) ~ 'darkgreen',
                                            grepl('^T', habit)  ~ 'darkgreen',
                                            grepl('^S', habit) & grepl('P', habit) ~ 'darkgreen',
                                            grepl('^S', habit) & grepl('F', habit) ~ 'darkgreen',
                                            grepl('^S', habit) & grepl('BE', habit) ~ 'darkgreen',
                                            grepl('^S', habit) ~ 'darkgreen',
                                            grepl('FE', habit) ~ 'darkgreen',
                                            grepl('^H', habit) & grepl('F', habit) ~ 'darkgreen',
                                            grepl('^H', habit) & type %in% 'grass/grasslike' ~ '#4D8000'),

                       stshape0 = case_when(grepl('^T', habit) & grepl('N', habit) ~ 'trunk',
                                           grepl('^T', habit)  ~ 'trunk',
                                           grepl('^S', habit)  ~ 'sticks',
                                           grepl('FE', habit) ~ NA,
                                           grepl('F', habit) ~ NA,
                                           type %in% 'grass/grasslike' ~ NA),
                       stfill0 = case_when(grepl('^T', habit) & grepl('N', habit) ~ 'orange',
                                           grepl('^T', habit)  ~ 'orange',
                                           grepl('^S', habit)  ~ 'orange',
                                           grepl('FE', habit) ~ NA,
                                           grepl('F', habit) ~ NA,
                                           type %in% 'grass/grasslike' ~ NA),
                       stcolor0 = case_when(grepl('^T', habit) & grepl('N', habit) ~ 'brown',
                                            grepl('^T', habit)  ~ 'brown',
                                            grepl('^S', habit)  ~ 'brown',
                                            grepl('FE', habit) ~ NA,
                                            grepl('F', habit) ~ NA,
                                            type %in% 'grass/grasslike' ~ NA),
                       fun=case_when(grepl('^T', habit)  ~ 'T',
                                     grepl('^S', habit)  ~ 'S',
                                     grepl('^H', habit)  ~ 'H'),
                       stems = round(plength*pwidth/10000*density,0) #count number of stem objects required plot size.
  )
#Check if user defined values exist for shape, fill, and color.
  if('crshape' %in% colnames(veg)){
    veg <- veg |> mutate(crshape = ifelse(is.na(crshape), crshape0, crshape), crshape0=NULL)}else{
      veg <- veg |> mutate(crshape = crshape0, crshape0=NULL)}
  if('crfill' %in% colnames(veg)){
    veg <- veg |> mutate(crfill = ifelse(is.na(crfill), crfill0, crfill), crfill0=NULL)}else{
      veg <- veg |> mutate(crfill = crfill0, crfill0=NULL)}
  if('crcolor' %in% colnames(veg)){
    veg <- veg |> mutate(crcolor = ifelse(is.na(crcolor), crcolor0, crcolor), crcolor0=NULL)}else{
      veg <- veg |> mutate(crcolor = crcolor0, crcolor0=NULL)}

  if('stshape' %in% colnames(veg)){
    veg <- veg |> mutate(stshape = ifelse(is.na(stshape), stshape0, stshape), stshape0=NULL)}else{
      veg <- veg |> mutate(stshape = stshape0, stshape0=NULL)}
  if('stfill' %in% colnames(veg)){
    veg <- veg |> mutate(stfill = ifelse(is.na(stfill), stfill0, stfill), stfill0=NULL)}else{
      veg <- veg |> mutate(stfill = stfill0, stfill0=NULL)}
  if('stcolor' %in% colnames(veg)){
    veg <- veg |> mutate(stcolor = ifelse(is.na(stcolor), stcolor0, stcolor), stcolor0=NULL)}else{
      veg <- veg |> mutate(stcolor = stcolor0, stcolor0=NULL)}


  strats <- veg |> subset(fun %in% c('T','S','H') & stems > 0) |> arrange(plot,-ht.max, -cover)#reduce to strata which have models and are not empty of stems
  strats$seq <- c(1:nrow(strats))
  strats <- strats |> group_by(plot) |> mutate(seqmin = min(seq), seq = seq-seqmin+1, seqmin = NULL)


  #Establish a stand 50 by 20 m.
  stand <- make_hex_stand(plength/100,1) |> subset(yp >= mean(yp)-pwidth/2 & yp < mean(yp)+pwidth/2) |> mutate(wtn = wt, stratid = NA)

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
    colors0 <- subset(thistrat, select=c(crfill, stfill, crcolor, stcolor))
    plant0 <- merge(plant0,colors0) |> mutate(fill = ifelse(obj %in% c('crown','herb'), crfill, stfill),
                                              color = ifelse(obj %in% c('crown','herb'), crcolor, stcolor),
                                              crfill = NULL, stfill = NULL, crcolor = NULL, stcolor = NULL)
    if(i==1){plants <- plant0}else{plants <- rbind(plants,plant0)}
  }

  #randomize sizes and positions
  plants <- plants |> group_by(stumpid) |>
    mutate(ht.max = max(z), crwd = max(x)-min(x),
           xpp = xp + runif(1, min = -0.8, max = 0.8),#shift position on grid
           zr = rnorm(1,(ht.max+0.01), (ht.max+0.01)/10)/(ht.max+0.01),#deviation in height # added 1 cm so that super short plants don't disappear.
           xr = (rnorm(1,(ht.max+0.01), (ht.max+0.01)/10)/(ht.max+0.01)+rnorm(1,crwd, crwd/10)/crwd)/2,#deviation in width partially related to height
           xn = x*xr+xpp,#resized width and put on new position
           zn = z*zr*(1-1/15))#resized height adjusted downward show that variation is less than max height in the field

  return(plants)
}
