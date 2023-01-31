#This function takes veg plot inported from NASIS (veg.spp <- soilDB::get_vegplot_species_from_NASIS_db()) and consolidates the redundant cover and height fields, and fills in missing heights.


clean.veg <- function(x){
  # plant.hts <- read.delim('data/plants/Plant_heights.txt')

  # x <- subset(x, select = c(vegplotid, plantsym, plantsciname, planttypegroup, plantnativity, plantheightcllowerlimit, plantheightclupperlimit, livecanopyhtbottom, livecanopyhttop,
  #                                 overstorydbhmin, overstorydbhmax, speciestraceamtflag,
  #                                 speciescancovpct, speciescancovclass, speciescomppct, speciescompbywtpct,
  #                                 akstratumcoverclass, akstratumcoverclasspct))

  x <- x %>% left_join(plant.hts, by=c('plantsciname'='Scientific.Name')) %>%
    left_join(subset(taxon.habits, select=c(Scientific.Name, GH)), by=c('plantsciname'='Scientific.Name')) %>%
    left_join(subset(gho, select=c(Revised.Symbol, Habitname,ESIS.Group)), by=c('GH'='Revised.Symbol'))
  x <- x %>% mutate(Ht_m = case_when(
    !is.na(Ht_m) ~ Ht_m,
    planttypegroup %in%  "tree" ~ 25,
    planttypegroup %in%  "shrub/vine" ~ 3,
    planttypegroup %in%  "forb" ~ 0.6,
    planttypegroup %in%  "grass/grasslike" ~ 0.6,
    planttypegroup %in%  "moss" ~ 0,
    TRUE ~ 0))
  x <- x %>% mutate(
    cover = case_when(
      !is.na(akstratumcoverclasspct) ~ as.numeric(akstratumcoverclasspct),
      !is.na(speciescancovpct) ~ as.numeric(speciescancovpct) + ifelse(speciestraceamtflag,0.2,0),
      speciescancovclass %in% "trace" ~ (0.1)/2,
      speciescancovclass %in% "0.1 to 1%" ~ (0.1+1)/2,
      speciescancovclass %in% "1.1 to 2%" ~ (1+2)/2,
      speciescancovclass %in% "2 to 5%" ~ (2+5)/2,
      speciescancovclass %in% "6 to 10%" ~ (5+10)/2,
      speciescancovclass %in% "11 to 25%" ~ (10+25)/2,
      speciescancovclass %in% "26 to 50%" ~ (25+50)/2,
      speciescancovclass %in% "51 to 75" ~ (50+75)/2,
      speciescancovclass %in% "76 to 95%" ~ (75+95)/2,
      speciescancovclass %in% "> 95%" ~ (95+100)/2,
      !is.na(speciescomppct) ~ as.numeric(speciescomppct),
      TRUE ~ 0),

    ht.min = 0,

    ht.max = case_when(
      !is.na(livecanopyhttop) ~ ht.metric(livecanopyhttop),
      !is.na(plantheightclupperlimit) ~ pmin(ht.metric(plantheightclupperlimit),
                                             pmax(Ht_m, ht.metric(plantheightcllowerlimit) +
                                                    pmax(Ht_m, ht.metric(plantheightcllowerlimit)/5))),
      akstratumcoverclass %in% "tree regeneration generally less than 4.5 m (15 ft) tall" ~ 4.5,
      akstratumcoverclass %in% "stunted tree generally less than 4.5 m (15 ft) tall" ~ 4.5,
      akstratumcoverclass %in% "medium tree generally between 4.5 and 12 m (15 and 40 ft) tall" ~ 12,
      akstratumcoverclass %in% "tall tree generally greater than 12 m (40 ft) tall" ~
        pmax(Ht_m*0.9, 12+3),
      akstratumcoverclass %in% "dwarf shrub layer less than about 20 cm (8 in) tall" ~ 0.2,
      akstratumcoverclass %in% "low shrub between about 20 and 100 cm (8 and 36 in) tall" ~ 1,
      akstratumcoverclass %in% "medium shrub between about 1 and 3 m (3 and 10 ft) tall" ~ 3,
      akstratumcoverclass %in% "tall shrub greater than about 3 m (10 ft) tall" ~
        pmin(5,pmax(Ht_m*0.9, 3+1)),
      akstratumcoverclass %in% "low and dwarf graminoid less than about 10 cm (4 in) tall" ~ 0.1,
      akstratumcoverclass %in% "medium graminoid between about 10 and 60 cm (4 and 24 in) tall" ~ 0.6,
      akstratumcoverclass %in% "tall graminoid generally greater than 60 cm (24 in) tall" ~
        pmin(2,pmax(Ht_m*0.9, 0.6+0.6)),
      akstratumcoverclass %in% "low and dwarf forb generally less than 10 cm (4 in) tall" ~ 0.1,
      akstratumcoverclass %in% "medium forb between about 10 and 60 cm (4 and 24 in) tall" ~ 0.6,
      akstratumcoverclass %in% "tall forb generally greater than 60 cm (24 in) tall" ~
        pmin(2,pmax(Ht_m*0.9, 0.6+0.6)),
      akstratumcoverclass %in% "mosses" ~ 0,
      TRUE ~ NA_real_),

    ht.min = case_when(
      !is.na(livecanopyhtbottom) ~ ht.metric(livecanopyhtbottom),
      TRUE ~ ht.max/2),

    ht.max = ht.round(ht.max),
    ht.min = ht.round(ht.min),
    diam.min = diam.metric(overstorydbhmin),
    diam.max = diam.metric(overstorydbhmax))
  colnames(x)
  x <- x %>% subset(select= c("vegplotid","plantsym","plantsciname","GH","Habitname","ESIS.Group","planttypegroup",
                              "plantnativity","cover","ht.min","ht.max","diam.min","diam.max"))
  return(x)
}

