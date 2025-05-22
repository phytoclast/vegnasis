library(soilDB)

get_plants_from_NASIS <- function(SS=TRUE, stringsAsFactors = NULL, dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  q <- paste("SELECT p.plantsciname, p.plantsym, a.areasymbol, a.areaname
  FROM plant_View_1 p
  LEFT OUTER JOIN paoccurrence pa ON pa.plantiidref = p.plantiid
             LEFT OUTER JOIN area a ON a.areaiid = pa.areaiidref
             ORDER BY p.plantsciname, a.areasymbol;"
  )
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # exec query
  d.project <- dbQueryNASIS(channel, q)

  # test is selected set is empty
  if (nrow(d.project) == 0)
    message("Your selected set or local database is missing data in the project table, please load it and try again")

  # uncode metadata domains
  d.project <- uncode(d.project, dsn = dsn)
  # done
  return(d.project)
}


plts <- get_plants_from_NASIS(SS=F)
plst <- plts |> subset(!is.na(areasymbol), select=c(plantsym, areasymbol))

saveRDS(plst, 'plst.RDS')

plst <- readRDS('plst.RDS')
usethis::use_data(plst)
State = "IN"
veg.raw <- vegnasis::veg.raw20250414
veg <- clean.veg(veg.raw)
vegst <-  subset(veg, grepl('2024MI',plot))
taxa <- vegst$taxon

check.phytogeography <- function(taxa,State){
  u <- subset(plst, areasymbol %in% State)
  u <- u |> mutate(taxon = fill.taxon.from.symbols(plantsym), utaxon = extractTaxon(harmonize.taxa(taxon, sensu = 'usda'), report ='binomial'))

  vtaxon <- extractTaxon(harmonize.taxa(taxa, sensu = 'usda'),report ='binomial')
  v <- data.frame(taxa=taxa,vtaxon=vtaxon)
  v <- v |> mutate(habit = get.habit.code(vtaxon), documented = ifelse(vtaxon %in% u$utaxon, 'yes','not'),
                   documented = ifelse(documented %in% 'not' & grepl('^N',habit) | is.na(taxa) | grepl('×', taxa), 'unknown',documented))
  return(v$documented)
}

vegst <- vegst |> mutate(doc = check.phytogeography(taxon,'NC'))


