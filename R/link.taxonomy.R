#link taxonomy ----
#This function parses genus from taxa and links to family according to APG IV taxonomy (vascular plants only). If binomial is phylogenetically inconsistent with the type species of the genus, the family will be incorrect. Harmonizing taxa first is recommended.
# taxa=veg$taxon
link.taxonomy <- function(taxa, taxrank=2){
  x  <-  data.frame(taxa=taxa) |> mutate(genus = str_split_fixed(taxa , '[[:blank:]]',3)[,1])
  x  <-  x |> left_join(familylink)
  x  <-  x |> left_join(apg)

  rankname <- case_when(taxrank == 9 ~ 'phylum',
                        taxrank == 8 ~ 'subphylum',
                        taxrank == 7 ~ 'superclass',
                        taxrank == 6 ~ 'class',
                        taxrank == 5 ~ 'subclass',
                        taxrank == 4 ~ 'superorder',
                        taxrank == 3 ~ 'order',
                        taxrank == 2 ~ 'family',
                        taxrank == 1 ~ 'genus',
                        TRUE ~ 'taxa')
     return(x[,rankname])
  }
