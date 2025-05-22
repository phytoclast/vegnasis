#' Check USDA Phytogeography
#' @description This tool checks a list of species against a known state distribution, documented with vouchers deposited in a herbarium. If the plant comes up not documented, it is possible that plant was identified incorrectly or the wrong PLANTS symbol was used. But it is also possible that the plant represents a new record of the species in that state, and should be vouchered. At this time, only USDA PLANTS database as of 2025 is consulted. Checking  local state flora databases or alternative websites like bonap.net is recommended to be certain as taxonomic opinions may vary. Non-vascular plants and hybrids are excluded from consideration due to less comprehensive documentation.
#' @param taxa Vector of taxon names
#' @param State State postal abbreviation
#'
#' @return Vector of statuses: "yes" = documented in the state; "not" = not documented in the state; "unknown" = distribution unknown.
#' @export
#'
#' @examples veg.raw <- vegnasis::veg.raw20250414
#' @examples veg <- clean.veg(veg.raw)
#' @examples vegst <-  subset(veg, grepl('2024MI',plot))#check Michigan plot records
#' @examples vegst <- vegst |> mutate(doc = check.phytogeography(taxon,'NC'))#check against North Carolina plants
#' @examples print(vegst[,c('plot','taxon','doc')])
check.phytogeography <- function(taxa,State){
  plst <- vegnasis::plst
  u <- subset(plst, areasymbol %in% State)
  u <- u |> mutate(taxon = fill.taxon.from.symbols(plantsym), utaxon = extractTaxon(harmonize.taxa(taxon, sensu = 'usda'), report ='binomial'))

  vtaxon <- extractTaxon(harmonize.taxa(taxa, sensu = 'usda'),report ='binomial')
  v <- data.frame(taxa=taxa,vtaxon=vtaxon)
  v <- v |> mutate(habit = get.habit.code(vtaxon), documented = ifelse(vtaxon %in% u$utaxon, 'yes','not'),
                   documented = ifelse(documented %in% 'not' & grepl('^N',habit) | is.na(taxa) | grepl('×', taxa), 'unknown',documented))
  return(v$documented)
}
