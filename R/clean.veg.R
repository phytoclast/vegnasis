#This function takes veg plot inported from NASIS (veg.spp <- soilDB::get_vegplot_species_from_NASIS_db()) and consolidates the redundant cover and height fields, and fills in missing heights.


clean.veg <- function(x){
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


    stratum.max = case_when(
      !is.na(plantheightclupperlimit) ~ ht.metric(plantheightclupperlimit),
      akstratumcoverclass %in% "tree regeneration generally less than 4.5 m (15 ft) tall" ~ 4.5,
      akstratumcoverclass %in% "stunted tree generally less than 4.5 m (15 ft) tall" ~ 4.5,
      akstratumcoverclass %in% "medium tree generally between 4.5 and 12 m (15 and 40 ft) tall" ~ 12,
      akstratumcoverclass %in% "tall tree generally greater than 12 m (40 ft) tall" ~ NA_real_,
      akstratumcoverclass %in% "dwarf shrub layer less than about 20 cm (8 in) tall" ~ 0.2,
      akstratumcoverclass %in% "low shrub between about 20 and 100 cm (8 and 36 in) tall" ~ 1,
      akstratumcoverclass %in% "medium shrub between about 1 and 3 m (3 and 10 ft) tall" ~ 3,
      akstratumcoverclass %in% "tall shrub greater than about 3 m (10 ft) tall" ~ NA_real_,
      akstratumcoverclass %in% "low and dwarf graminoid less than about 10 cm (4 in) tall" ~ 0.1,
      akstratumcoverclass %in% "medium graminoid between about 10 and 60 cm (4 and 24 in) tall" ~ 0.6,
      akstratumcoverclass %in% "tall graminoid generally greater than 60 cm (24 in) tall" ~ NA_real_,
      akstratumcoverclass %in% "low and dwarf forb generally less than 10 cm (4 in) tall" ~ 0.1,
      akstratumcoverclass %in% "medium forb between about 10 and 60 cm (4 and 24 in) tall" ~ 0.6,
      akstratumcoverclass %in% "tall forb generally greater than 60 cm (24 in) tall" ~ NA_real_,
      akstratumcoverclass %in% "mosses" ~ 0,
      TRUE ~ NA_real_),

    stratum.min = case_when(
      !is.na(plantheightcllowerlimit) ~ ht.metric(plantheightcllowerlimit),
      akstratumcoverclass %in% "tree regeneration generally less than 4.5 m (15 ft) tall" ~ 0,
      akstratumcoverclass %in% "stunted tree generally less than 4.5 m (15 ft) tall" ~ 0,
      akstratumcoverclass %in% "medium tree generally between 4.5 and 12 m (15 and 40 ft) tall" ~ 4.5,
      akstratumcoverclass %in% "tall tree generally greater than 12 m (40 ft) tall" ~ 12,
      akstratumcoverclass %in% "dwarf shrub layer less than about 20 cm (8 in) tall" ~ 0,
      akstratumcoverclass %in% "low shrub between about 20 and 100 cm (8 and 36 in) tall" ~ 0.2,
      akstratumcoverclass %in% "medium shrub between about 1 and 3 m (3 and 10 ft) tall" ~ 1,
      akstratumcoverclass %in% "tall shrub greater than about 3 m (10 ft) tall" ~ 3,
      akstratumcoverclass %in% "low and dwarf graminoid less than about 10 cm (4 in) tall" ~ 0,
      akstratumcoverclass %in% "medium graminoid between about 10 and 60 cm (4 and 24 in) tall" ~ 0.1,
      akstratumcoverclass %in% "tall graminoid generally greater than 60 cm (24 in) tall" ~ 0.6,
      akstratumcoverclass %in% "low and dwarf forb generally less than 10 cm (4 in) tall" ~ 0,
      akstratumcoverclass %in% "medium forb between about 10 and 60 cm (4 and 24 in) tall" ~ 0.1,
      akstratumcoverclass %in% "tall forb generally greater than 60 cm (24 in) tall" ~ 0.6,
      akstratumcoverclass %in% "mosses" ~ 0,
      TRUE ~ NA_real_),

    crown.min = ht.metric(livecanopyhtbottom),
    crown.max = ht.metric(livecanopyhttop),

    diam.min = diam.metric(overstorydbhmin),
    diam.max = diam.metric(overstorydbhmax),
    plot = vegplotid,
    taxon = plantsciname,
    type = planttypegroup,
    nativity = plantnativity)

  x <- x %>% subset(select= c("plot","plantsym","taxon","type",
                              "nativity","cover","stratum.min","stratum.max","crown.min","crown.max","diam.min","diam.max"))
  return(x)
}