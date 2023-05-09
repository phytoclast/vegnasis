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
