# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(vegnasis)
PLANTS <- read.csv('data_raw/usdaplantsym.txt')
USDAfams <- read.csv('data_raw/USDAfams.csv')
USDAunk <- read.csv('data_raw/usdaunksym.txt')
PLANTS <- PLANTS |> mutate(sym = ifelse(is.na(Synonym.Symbol)|Synonym.Symbol %in% '', Symbol, Synonym.Symbol), taxon = extractTaxon(Scientific.Name.with.Author), author = extractTaxon(Scientific.Name.with.Author, 'author'))


PLANTS.illegit <- PLANTS |> subset(grepl('auct.',author) |
                                     grepl('illeg.',author) |
                                     grepl(' non',author) ,
                                   select=c(sym, taxon, author))
usdaplants <- PLANTS |> subset(!sym %in% PLANTS.illegit$sym, select=c(sym, taxon, author))
colnames(USDAfams) <- c("sym","taxon")
colnames(USDAunk) <- c("sym","taxon")
additional <- data.frame(sym='PRENA',taxon='Nabalus')
usdaplants <- usdaplants |> dplyr::bind_rows(USDAfams) |> dplyr::bind_rows(USDAunk)|> dplyr::bind_rows(additional)
usethis::use_data(usdaplants, overwrite = T)


obs <- read.delim('data_raw/Sites.txt')
obssp <- read.delim('data_raw/Observed_Species.txt')

obs <- subset(obs, Latitude != 0 & Observer_Code %in% c('BEL.JH', 'TOL.NB', 'GRR.NJL', 'GRR.GJS') &
                Year >=2011 & !Observation_Type %in% c('Bogus', 'Floristics'))

obsspp <- subset(obssp, Observation_ID %in% obs$Observation_ID)

usethis::use_data(obs, overwrite = T)
usethis::use_data(obsspp, overwrite = T)
#clean taxon habits
library(vegnasis)
taxon.habits <- read.csv('data_raw/taxon.habits.csv', encoding = 'latin1')
taxon.habits <- taxon.habits |> mutate(Scientific.Name = cleanEncoding(Scientific.Name), genus = cleanEncoding(genus))
taxon.habits <- taxon.habits |> mutate(Scientific.Name = extractTaxon(Scientific.Name), genus = extractTaxon(Scientific.Name, 'genus'))

# taxon.habits1 <-  vegnasis::taxon.habits
usethis::use_data(taxon.habits, overwrite = T)

#new genus habits
library(vegnasis)
genus.habits <- read.csv('data_raw/genus.habits.csv', encoding = 'latin1')
nvagenustaxonomy <- read.csv('data_raw/nvagenustaxonomy.csv')
nvagenustaxonomy <- nvagenustaxonomy |> mutate(GH = case_when(grepl('algae|Cyano', type) ~ 'N.A',
                                                              grepl('bryophyte', type) ~ 'N.B',
                                                              grepl('lich', type) ~ 'N.L'),
                                               ht.max = 0)

genus.habits <- subset(genus.habits, !genus %in% nvagenustaxonomy$genus & !grepl('^N',GH))
genus.habits <- rbind(genus.habits, nvagenustaxonomy[,colnames(genus.habits)])



#usethis::use_data(taxon.habits, overwrite = T)

#expand habits of synonym genera
taxon.habits <-  vegnasis::taxon.habits

syns3 <- vegnasis::syns3
syns3 <- mutate(syns3, gbif = case_when(!is.na(bonap) ~ bonap,
                                        !is.na(kew) ~ kew,
                                        !is.na(wplants) ~ wplants,
                                        !is.na(usda) ~ usda,
                                        TRUE ~ gbif))
synshabits <- syns3[,c('taxon', 'gbif')] |> left_join(taxon.habits[,c("Scientific.Name", 'Stem', 'GH','ht.max')], by=join_by(gbif==Scientific.Name))
synshabits$genus <- extractTaxon(synshabits$taxon, 'genus')
synshabits <- synshabits |> subset(!is.na(Stem)) |> group_by(genus, Stem, GH) |> mutate(n = length(taxon)) |> group_by(genus, Stem) |> mutate(p=n/max(n), maxp=max(p), n2 = length(taxon)) |>
  group_by(genus) |> mutate(p2=n2/max(n2), maxp2=max(p2), ht.max=mean(ht.max)) |> ungroup()
synshabits <- synshabits |> mutate(keep = ifelse(maxp==p & maxp >= 0.9,1,0), keep2 = ifelse(maxp2==p2 & maxp2 >= 0.9,1,0)) |> group_by(genus) |> mutate(maxkeep = max(keep)*max(keep2)) |> ungroup()
synshabits <- synshabits |> subset(keep==1 & keep2==1 & !genus %in% genus.habits$genus, select = c(genus, GH, ht.max)) |> unique()
synshabits <- synshabits |> mutate(ht.max = vegnasis::ht.round(ht.max))

genus.habits <- genus.habits |> rbind(synshabits) |> unique()

ghab2 <- syns3 |> mutate(genus = extractTaxon(taxon, 'genus'),acgenus = extractTaxon(gbif, 'genus'))
ghab2 <- ghab2[,c('taxon', 'gbif', 'genus','acgenus')] |> left_join(genus.habits, by=join_by(acgenus==genus))
ghab2 <- ghab2 |> group_by(genus, GH) |> mutate(n = length(taxon)) |> group_by(genus) |> mutate(p=n/max(n), maxp=max(p)) |> ungroup()

ghab2 <- ghab2 |> subset(grepl('^N',GH) & p >= 0.9 & p==maxp & !genus %in% genus.habits$genus, select = c(genus, GH, ht.max)) |> unique()
genus.habits <- genus.habits |> rbind(ghab2) |> unique()
g <- rbind(c('Leucothrinax', 'T.P',0),
      c('Flavocetraria', 'N.L',0),
      c('Oreopteris', 'H.FE',.5),
      c('Cladopodiella', 'N.B',0),
      c('Aulocomnium', 'N.B',0),
      c('Limprichtia', 'N.B',0),
      c('Eualaria', 'N.A',0),
      c('Victoria', 'H2A',.1)
)
colnames(g) <- colnames(genus.habits)
genus.habits <- genus.habits |> rbind(g)

usethis::use_data(genus.habits, overwrite = T)




#new taxonomy
library(vegnasis)
apg <- read.csv('data_raw/apg.csv', encoding = 'latin1')
apg$kingdom <- 'Plantae'
apg$APG_IV_sort <- apg$APG_IV_sort + 800000
apg <- apg[,c("APG_IV_sort","kingdom", "phylum","subphylum","superclass","class","subclass","superorder","order","family")]
nvafamily <- read.csv('data_raw/nvafamilytaxonomy.csv') |> arrange(kingdom, phylum, class, order, family)
nvafamily <- nvafamily |> mutate(subphylum=NA, superclass=NA, subclass=NA, superorder=NA)
nvafamily <- nvafamily |> group_by(kingdom,phylum) |> mutate(n = length(family), nsort=1:(n)[1]) |> ungroup()
nvafamily <- nvafamily |> mutate(sort2 = case_when(phylum %in% 'Cyanobacteria' ~ 1,
                                                   phylum %in% 'Rhodophyta' ~ 2,
                                                   phylum %in% 'Chlorophyta' ~ 3,
                                                   phylum %in% 'Charophyta' ~ 4,
                                                   phylum %in% 'Anthocerotophyta' ~ 5,
                                                   phylum %in% 'Marchantiophyta' ~ 6,
                                                   phylum %in% 'Bryophyta' ~ 7,
                                                   phylum %in% 'Tracheophyta' ~ 8,
                                                   phylum %in% 'Ochrophyta' ~ 9,
                                                   phylum %in% 'Ascomycota' ~ 10,
                                                   ),
                                 APG_IV_sort = sort2*100000+nsort)
apg <- rbind(apg, nvafamily[,colnames(apg)]) |> arrange(APG_IV_sort)

usethis::use_data(apg, overwrite = T)

#new synonymy table 2026-07-13
library(vegnasis)
# syns2 <- read.csv('data_raw/syn2.csv', encoding = 'UTF-8')
syns3 <- readRDS('data_raw/syns3.RDS')
nva <- read.csv('data_raw/nvanomenclature.csv')
syns3$gbif=NA
nva <- nva |> mutate(author=auth, kew=NA,bonap=NA,wplants=NA) |> subset(select = colnames(syns2))
syns3 <- syns3 |> subset(!taxon %in% nva$taxon)
syns3 <- rbind(syns3, nva)
syns3 <- syns3 |> mutate(usda = ifelse(nchar(usda) < 1, NA,usda), gbif = ifelse(nchar(gbif) < 1, NA,gbif))
#syns3 <- syns3 |> group_by(taxon) |> mutate(n=length(taxon)) |> ungroup()
usethis::use_data(syns3, overwrite = T)

library(vegnasis)
obssites <- vegnasis::obs
obstaxa <- vegnasis::obsspp

veg=clean.veg.log(obssites, obstaxa)

veg1 = as.VegLog(veg[1:10,])
veg2 = as.VegLog(veg[50:60,])
veg3 = c(veg1,veg2)

#family link
library(vegnasis)
# familylink <- read.csv('familylink.csv', encoding = 'UTF-8')

# familylink <- familylink[-c(13820,13771),]
# familylink <- familylink |> rbind(data.frame(family=c('Cactaceae','Cactaceae'), genus=c('X Pachgerocereus','X Pachebergia'), ac=TRUE))
# # familylink <- familylink |> mutate(genus = ifelse(grepl('Pachgerocereus',genus), 'X Pachgerocereus',genus))
# # familylink <- familylink |> mutate(genus = ifelse(grepl('Pachebergia',genus), 'X Pachebergia',genus))
# familylink <- familylink |> subset(!(family %in% 'Taxaceae' & genus %in% 'Cephalotaxus'))
# familylink <- familylink |> mutate(family = ifelse(genus %in% c('Calatola', 'Oecopetalum', 'Ottoschulzia'), 'Metteniusaceae',family))
# familylink <- subset(familylink, !(genus %in% 'Banisteria' & family %in% 'Rhamnaceae'))
# familylink <- familylink |> mutate(genus = extractTaxon(genus)) |> subset(select=c(family,genus)) |> unique()
# new <- subset(genfams, is.na(family.kew) & !is.na(family), select = c(family, genus.kew)) |> unique()
# colnames(new) <- c('family', 'genus')
# new <- new |> mutate(family = ifelse(genus %in% c('×Dryostichum'), 'Dryopteridaceae',family))
# familylink <- familylink |> rbind(new) |> unique()
# write.csv(familylink, 'familylink2.csv', fileEncoding = 'UTF-8', row.names = FALSE)
familylink <- read.csv('data_raw/familylink2.csv', encoding = 'UTF-8')
nvagenustaxonomy <- read.csv('data_raw/nvagenustaxonomy.csv')
familylink <- subset(familylink, !genus %in% nvagenustaxonomy$genus)
familylink <- familylink |> rbind(nvagenustaxonomy[,colnames(familylink)]) |> unique()


# gf <- familylink |> group_by(genus) |> mutate(ct = length(genus))

usethis::use_data(familylink, overwrite = T)

c("Site_Type", "Project_ID", "Observation_ID","Observer_Code",
"Observation_Serial","Observation_Description","Observation_Label","Observation_Type",
"Latitude",  "Longitude", "Error",     "Date",
"Prefix",    "Year",      "Mon",       "Day",
"Nation",    "State",     "County",    "Island",
"FIPS", "Elevation", "Community_Name", "Community_Description",
"Structure", "Landscape", "HillslopePosition", "Position",
"Aspect", "Slope", "PlotBearing", "Map.Unit",
"Soil.Series", "Soil.Taxon", "Drainage.Class", "Restriction_Depth",
"WT_Depth",  "MinWT_Depth", "Soil_Notes", "Litter_Cover",
"Snag_Count", "Snag_Diam", "Plot_Area_m", "BA_Count",
"BA_Factor", "Cowardin",  "HGM",       "f1",
"f2",        "f3",        "s1",        "s2",
"s3",        "t1",        "t2",        "t3",
"t4",        "Tree_Cover", "Subcanopy_Cover", "TallShrub_Cover",
"Shrub_Cover", "Herb_Cover", "Moss_Cover", "Aggregated_Overstory",
"Aggregated_Subcanopy", "Aggregated_TallShrub", "Aggregated_Shrub", "Aggregated_Understory",
"Estimated_Understory", "DWD_Hits1", "DWD_Hits2", "DWD_Hits3",
"DWD_Hits4", "DWD_Hits5", "Transect_Length", "Log_Cover",
"Rock_Cover", "Lichen_Cover", "Water_Cover", "MaxWater_Cover",
"DBH_lower", "DBH_upper", "Canopy_lower", "Canopy_upper",
"User_Pedon_ID", "User_Plot_ID", "SoilTexture", "TPI",
"Upper",     "Middle",    "Lower",     "Coastal",
"Floodplain", "Inland",    "Hydric",    "Nonhydric",
"Aquatic",   "Wet",       "Moist",     "Dry",
"Mucky",     "Rocky",     "Sandy",     "Loamy",
"Calcareous", "Euic",      "Dysic",     "Salty",
"Fresh",     "Natural",   "Seminatural", "Cultural",
"MLRA",      "Cold",      "Cool",      "Mild",
"Warm",      "Hot",       "Humid",     "Subhumid",
"Arid","Microthermal", "Mesothermal", "Megathermal")
