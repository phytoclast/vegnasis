
syns3 <- readRDS('data_raw/syns3.RDS')

vib <- subset(syns3, taxon %in% 'Viburnum rafinesqueanum')
vib$taxon <- 'Viburnum rafinesquianum'
syns3 <- syns3 |> dplyr::mutate(usda = ifelse(taxon %in% 'Festuca trachyphylla', 'Festuca brevipila',usda))
syns3 <- syns3 |> rbind(vib) |> unique()

saveRDS(syns3, 'data_raw/syns3.RDS')
usethis::use_data(syns3, overwrite = T)
