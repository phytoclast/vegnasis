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
taxon.habits <- read.csv('data_raw/taxon.habits.csv', encoding = 'UTF-8')
taxon.habits <- taxon.habits |> mutate(Scientific.Name = cleanEncoding(Scientific.Name), genus = cleanEncoding(genus))
taxon.habits <- taxon.habits |> mutate(Scientific.Name = extractTaxon(Scientific.Name), genus = extractTaxon(Scientific.Name, 'genus'))
usethis::use_data(taxon.habits, overwrite = T)
#new synonymy table
library(vegnasis)
syns2 <- read.csv('data_raw/syns.csv', encoding = 'UTF-8')
usethis::use_data(syns2, overwrite = T)

library(vegnasis)
obssites <- vegnasis::obs
obstaxa <- vegnasis::obsspp

veg=clean.veg.log(obssites, obstaxa)

veg1 = as.VegLog(veg[1:10,])
veg2 = as.VegLog(veg[50:60,])
veg3 = c(veg1,veg2)



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
