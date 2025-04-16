#This function takes veg plot imported from NASIS (veg.spp <- soilDB::get_vegplot_species_from_NASIS_db()) and consolidates the redundant cover and height fields, and fills in missing heights.
#' Clean vegetation data imported from NASIS
#'
#' @description
#' This function takes veg plot imported from NASIS and consolidates the redundant cover and height fields.
#'
#'
#' @param x Species composition data frame derived from a soilDB NASIS import function.
#' @param type Should missing plant types (habits) be filled (TRUE/FALSE)?
#' @param hts Should missing plant heights be filled to allow assignment to strata (TRUE/FALSE)?
#'
#' @return Data frame containing a standardized set of quanitative vegetation parameters.
#' @export
#'
#' @examples veg.raw <- soilDB::get_vegplot_species_from_NASIS_db()
#' @examples veg <- clean.veg(veg.raw)
#'
clean.veg <- function(x, type=TRUE, hts=FALSE){
  if(type){
    x <- x |> mutate(planttypegroup = fill.type(plantsciname, type=planttypegroup))
    x <- x |> mutate(planttypegroup = case_when(
      is.na(planttypegroup) & plantsym %in% c("2CYAN","2ALGA","2AB","2AFW","2AG","2AM","2AR") ~ 'microbiotic crust',
      is.na(planttypegroup) & plantsym %in% c("2LICHN","2LC","2LCB","2LCE","2LCN","2LCGEL","2LCGON",
                                              "2LCP","2LCQ","2LCS","2LF","2LFL","2LFU","2LUV","2LU",
                                              "2LUF","2LUR") ~ 'lichen',
      is.na(planttypegroup) & plantsym %in% c("2BRY","2MOSS","2HORN",
                                              "2LW","2LWL","2LWT") ~ 'moss',
      is.na(planttypegroup) & plantsym %in% c("2FERN","2FORB","2FA","2FB","2FD",
                                              "2FDA","2FDB","2FDP","2FI","2FM","2FMA","2FMB",
                                              "2FMP","2FN","2FP","2FS","2FSA","2FSB","2FSP",
                                              "2VH","2VHA","2VHD","2VHDA","2VHDP","2VHM","2VHMA",
                                              "2VHMP","2VHP","2VHS","2VHSA","2VHSP","2VW") ~ 'forb',
      is.na(planttypegroup) & plantsym %in% c("2GRAM","2GA","2GB","2GI","2GN","2GP","2GW","2GL",
                                              "2GLA","2GLB","2GLP") ~ 'grass/grasslike',
      is.na(planttypegroup) & plantsym %in% c("2SHRUB","2SB","2SD","2SDB","2SDBD","2SDBM",
                                              "2SDN","2SE","2SEB","2SEBD","2SEBM","2SEN",
                                              "2SN","2S","2SS","2SSL","2SSS","2SUBS","2SSB",
                                              "2SSD","2SSDB","2SSDBD","2SSDBM","2SSDN",
                                              "2SSE","2SSEB","2SSEBD","2SSEBM","2SSEN",
                                              "2SSN","2SSS2","2SSSL","2SSSS",
                                              "2VWD","2VWDD","2VWDM","2VWE","2VWED","2VWEM") ~ 'shrub/vine',
      is.na(planttypegroup) & plantsym %in% c("2TREE","2TB","2TD","2TDB","2TDBD","2TDBM",
                                              "2TDN","2TE","2TEB","2TEBD","2TEBM","2TEN",
                                              "2TN","2TS","2TSL","2TSS") ~ 'tree',
      TRUE ~ planttypegroup))
  }

  x <- x %>% mutate(
    vegetationstratalevel =
      case_when(
        is.na(vegetationstratalevel) & is.na(akstratumcoverclass) & is.na(livecanopyhttop) &
          is.na(plantheightclupperlimit) & (!is.na(understorygrcovclass)|!is.na(understorygrcovpct)) &
          (is.na(speciescancovclass) & is.na(speciescancovpct))  ~ 'understory',
        is.na(vegetationstratalevel) & is.na(akstratumcoverclass) & is.na(livecanopyhttop) &
          is.na(plantheightclupperlimit) & planttypegroup %in% 'tree'  ~ 'overstory',
        TRUE ~ vegetationstratalevel),

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
      !is.na(understorygrcovpct)  ~ as.numeric(understorygrcovpct),
      understorygrcovclass %in% "trace to 1%" ~ (1)/2,
      understorygrcovclass %in% "2 to 9%" ~ (2+9)/2,
      understorygrcovclass %in% "10 to 19%" ~ (10+19)/2,
      understorygrcovclass %in% "20 to 29%" ~ (20+29)/2,
      understorygrcovclass %in% "30% or more" ~ (30+59)/2,
      !is.na(speciescomppct) ~ as.numeric(speciescomppct),
      !is.na(speciesbasalarea) ~ BA.to.cover(speciesbasalarea*10000/43560),
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
      vegetationstratalevel %in% "understory" ~ 5,
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
      vegetationstratalevel %in% "overstory" ~ 5,
      TRUE ~ NA_real_),

    crown.min = ht.metric(livecanopyhtbottom),
    crown.max = ht.metric(livecanopyhttop),

    dbh.min = dbh.metric(overstorydbhmin),
    dbh.max = dbh.metric(overstorydbhmax),

    plot = vegplotid,
    label = vegplotname,
    date = obsdate,
    symbol = plantsym,
    taxon = plantsciname,
    type = planttypegroup,
    nativity = plantnativity,
    #habit = NA_character_,
    BA = round(speciesbasalarea*10000/43560,1))

  x <- x %>% subset(select= c("plot","label","date","symbol","taxon","type",
                              "nativity","cover","stratum.min","stratum.max","crown.min","crown.max","dbh.min","dbh.max","BA"))

  if(hts){
    x <- fill.hts.df(x)
  }

  return(x)
}

#This function takes veg plot transect imported from NASIS (veg.spp <- soilDB::get_vegplot_species_from_NASIS_db()) and consolidates the redundant cover and height fields, and fills in missing heights.

clean.veg.transect <- function(x){
  x <- x %>% mutate(
    cover = case_when(

      !is.na(speciescancovpct) ~ as.numeric(speciescancovpct) + ifelse(speciestraceamtflag,0.2,0),
      speciescancovaveclass %in% "trace" ~ (0.1)/2,
      speciescancovaveclass %in% "0.1 to 1%" ~ (0.1+1)/2,
      speciescancovaveclass %in% "1.1 to 2%" ~ (1+2)/2,
      speciescancovaveclass %in% "2 to 5%" ~ (2+5)/2,
      speciescancovaveclass %in% "6 to 10%" ~ (5+10)/2,
      speciescancovaveclass %in% "11 to 25%" ~ (10+25)/2,
      speciescancovaveclass %in% "26 to 50%" ~ (25+50)/2,
      speciescancovaveclass %in% "51 to 75" ~ (50+75)/2,
      speciescancovaveclass %in% "76 to 95%" ~ (75+95)/2,
      speciescancovaveclass %in% "> 95%" ~ (95+100)/2,
      !is.na(speciescomppctdaubenmire) ~ as.numeric(speciescomppctdaubenmire),
      !is.na(speciescancovpctavedaub) ~ as.numeric(speciescancovpctavedaub),
      TRUE ~ NA_real_),

    foliar = case_when(!is.na(speciesfoliarcovhitcount) & !is.na(speciestotfoliarcovlineint) ~ as.numeric(speciesfoliarcovhitcount/speciestotfoliarcovlineint)*100,
                       !is.na(speciesfoliarcovpctlineint) ~ as.numeric(speciesfoliarcovpctlineint),
                       !is.na(speciescomppctlineintercept) ~ as.numeric(speciescomppctlineintercept),
                       TRUE ~ NA_real_),

    biomass = case_when(!is.na(speciesaveyielddblsamp) ~ biomass.metric(speciesaveyielddblsamp),
                        TRUE ~ NA_real_),
    rbiomass = case_when(!is.na(speciescomppctdblsamp) ~ as.numeric(speciescomppctdblsamp),
                         TRUE ~ NA_real_),

    stratum.max = case_when(
      !is.na(plantheightclupperlimit) ~ ht.metric(plantheightclupperlimit),
      TRUE ~ NA_real_),

    stratum.min = case_when(
      !is.na(plantheightcllowerlimit) ~ ht.metric(plantheightcllowerlimit),
      TRUE ~ NA_real_),

    crown.min = ht.metric(specieslivecanhtbotave),
    crown.max = ht.metric(specieslivecanhttopave),

    dbh.min = dbh.metric(overstorydbhmin),
    dbh.max = dbh.metric(overstorydbhmax),
    plot = vegplotid,
    label = vegplotname,
    # date = obsdate,
    symbol = plantsym,
    taxon = plantsciname,
    type = planttypegroup,
    nativity = plantnativity,
    BA = round(speciesbasalarea*10000/43560,1))

  x <- x %>% subset(select= c("plot","label","symbol","taxon","type",
                              "nativity","cover","foliar","biomass", "rbiomass","stratum.min","stratum.max","crown.min","crown.max","dbh.min","dbh.max","BA"))
  return(x)
}
