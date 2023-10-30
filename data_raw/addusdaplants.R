usdaplants <- read.csv('data_raw/plantssym.csv', encoding = 'latin1')
PLANTS <- read.csv('data_raw/PLANTSdownloadData.txt')
USDAfams <- read.csv('data_raw/USDAfams.csv')

PLANTS.illegit <- PLANTS |> subset(grepl('auct.',Genera.Binomial.Author) |
                                     grepl('illeg.',Genera.Binomial.Author) |
                                     grepl(' non',Genera.Binomial.Author) |
                                     grepl('auct',Trinomial.Author) |
                                     grepl('illeg.',Trinomial.Author) |
                                     grepl(' non',Trinomial.Author),
                                   select=c(Accepted.Symbol, Symbol, Scientific.Name, Genera.Binomial.Author, Trinomial.Author))
usdaplants <- usdaplants |> subset(!sym %in% PLANTS.illegit$Symbol)
colnames(USDAfams) <- c("sym","taxon")
usdaplants <- usdaplants |> dplyr::bind_rows(USDAfams)
usethis::use_data(usdaplants, overwrite = T)


obs <- read.delim('data_raw/Sites.txt')
obssp <- read.delim('data_raw/Observed_Species.txt')

obs <- subset(obs, Latitude != 0 & Observer_Code %in% c('BEL.JH', 'TOL.NB', 'GRR.NJL', 'GRR.GJS') &
                Year >=2011 & !Observation_Type %in% c('Bogus', 'Floristics'))

obssp <- subset(obssp, Observation_ID %in% obs$Observation_ID)

usethis::use_data(obs, overwrite = T)
usethis::use_data(obssp, overwrite = T)
