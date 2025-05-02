library(vegnasis)
library(climatools)
library(soilDB)
library(aqp)
library(tidyr)

condRound10 <- function(x){
  x <- ifelse(x < 0.05, 0, ifelse(x < 10, round(x, 1), round(x,0)))
  x <- as.character(x)
  return(x)
}
condRound1 <- function(x){
  x <-  ifelse(x < 0.05, 0, ifelse(x < 1, round(x, 1), round(x,0)))
  x <- as.character(x)
  return(x)
}

#set working directory to folder where this R file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# veg.raw0 <- soilDB::get_vegplot_species_from_NASIS_db(SS=F)
# saveRDS(veg.raw0,'jackpine/veg.raw0.RDS')

# landuse <- read.csv('jackpine/dsp_landuse.csv')
# landuse$landuse <- str_replace_all(landuse$landuse, '-','_')
# veg.raw <- readRDS('jackpine/veg.raw0.RDS')
siteass <- get_site_association_from_NASIS(SS=F)
sites <- get_site_data_from_NASIS_db(SS=F)
veg.raw <- soilDB::get_vegplot_species_from_NASIS_db(SS=F)
vegplot <- soilDB::get_vegplot_from_NASIS_db(SS=F)
vegground <- get_vegplot_groundsurface_from_NASIS_db(SS=F)

veg <- clean.veg(veg.raw)
veg <- subset(veg, !is.na(plot) & cover >= 5, select=c(plot)) |> unique()
veg.raw <- veg.raw |> subset(vegplotid %in% veg$plot)
vegplot <- vegplot |> subset(vegplotid %in% veg.raw$vegplotid)
sites <- sites |> subset(usiteid %in% vegplot$usiteid)
# saveRDS(siteass,'siteass20250414.RDS')
# saveRDS(vegground,'vegground20250414.RDS')
# saveRDS(sites,'sites20250414.RDS')
# saveRDS(veg.raw,'veg.raw20250414.RDS')
# saveRDS(vegplot,'vegplot20250414.RDS')
# siteass20250414 <- readRDS('data_raw/siteass20250414.RDS')
sites20250414 <- readRDS('data_raw/sites20250414.RDS')
vegground20250414 <- readRDS('data_raw/vegground20250414.RDS')
veg.raw20250414 <- readRDS('data_raw/veg.raw20250414.RDS')
vegplot20250414 <- readRDS('data_raw/vegplot20250414.RDS')
usethis::use_data(vegground20250414, overwrite = T)
usethis::use_data(siteass20250414, overwrite = T)
usethis::use_data(sites20250414, overwrite = T)
usethis::use_data(veg.raw20250414, overwrite = T)
usethis::use_data(vegplot20250414, overwrite = T)



#narrow to a project (user site association id)
thesesites <- subset(siteass, usiteassocid %in% 'DSP-F094AB019MI-2024')
sites <- subset(sites, usiteid %in% thesesites$usiteid)

landuse <- data.frame(siteobsiid=sites$siteobsiid, landuse = sites$commphasename)
landuse <- landuse |> left_join(data.frame(siteobsiid=vegplot$siteobsiid, plot = vegplot$vegplotid)) |> subset(!is.na(plot) & !is.na(landuse))
veg <- clean.veg(veg.raw)|> subset(!is.na(taxon))
veg <- veg |> inner_join(landuse)

veg <- veg |> mutate(type=NA) |> fill.type.df() |> fill.hts.df()
veg <- veg |> mutate(taxon = harmonize.taxa(veg$taxon, fix = TRUE, sensu = "usda"))

#Get vegetation Structure ----
veg.str <- veg |> get.structure(simple = TRUE)



veg.str <- veg.str |> inner_join(landuse)
veg.str.long <- tidyr::pivot_longer(veg.str, c(tree,shrub,herb,moss,ht.max))
veg.str.summary <- veg.str.long |> group_by(landuse, name) |> summarise(Low = round(quantile(value,0.05),1),
                                                                 RV = round(mean(value),1),
                                                                 High = round(quantile(value,0.95),1))

veg.str.wide <- tidyr::pivot_wider(veg.str.summary, names_from = landuse, values_from = c(Low,RV,High)) |> as.data.frame()

library(kableExtra)
library(knitr)
#kableextra ----
df2 <- veg.str.summary |> group_by(landuse) |> arrange()
df2$name <- factor(df2$name, levels = c('ht.max','tree','shrub','herb','moss'))
df2 <- df2[order(df2$landuse,df2$name),]
df2 |>
  knitr::kable(row.names = FALSE, digits = c(0,0,0,0)) |>
  remove_column(1) |>
  kableExtra::group_rows(index = table(df2$landuse)) |>
  # kableExtra::kable_paper("hover", full_width = F)
kable_classic(full_width = F, html_font = "Cambria")


#flextable ----
library(flextable)


pcolors <- palette.colors(length(unique(veg.str$landuse))+1, palette = "ggplot2")
pcolors <- pcolors[2:length(pcolors)]
pcolors <- c('white',pcolors,pcolors,pcolors)
thcols <- 1:length(unique(veg.str$landuse))
thcols <- 3*thcols+1
df2 <- veg.str.wide
df2$name <- factor(df2$name, levels = c('ht.max','tree','shrub','herb','moss'))
df2 <- df2[order(df2$name),]
df2$name <- c('Maximum Height (m)','Tree (%)','Shrub (%)','Herb (%)','Moss (%)')

repnames <- colnames(df2)
repnames <- stringr::str_split_fixed(repnames, '_', 2)
repnamesA <- repnames[,2]
repnamesB <- repnames[,1]
repdf <- data.frame(A=repnamesA, B=repnamesB, C=pcolors)
repdf <- repdf |> mutate(seq = 1:nrow(repdf))
repdf <- repdf |> arrange(A)
repnames <- paste0(repnames[,2],'_',repnames[,1])
colnames(df2) <- repnames
df2 <- df2[,repdf$seq]
colnames(df2)[1]<-'Variable'

theme_design <- function(x) {
  x <- border_remove(x)
  std_border <- fp_border_default(width = 0.5, color = "black")
  thk_border <- fp_border_default(width = 2, color = "black")
  x <- fontsize(x, size = 10, part = "all")
  x <- font(x, fontname = "Cambria", part = "all")
  x <- align(x, align = "center", part = "all")
  x <- bold(x, bold = TRUE, part = "all")
  x <- bg(x, bg = "white", part = "body")
  x <- bg(x, bg = repdf$C, part = "header")
  x <- bg(x, bg = "white", part = "footer")
  x <- color(x, color = "black", part = "all")
  x <- padding(x, padding = 1, part = "all")
  x <- border_outer(x, part="all", border = thk_border )
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
  x <- vline(x, j = c(1,thcols), border = thk_border, part = "all")
  x <- set_table_properties(x, layout = "fixed")
  x
}
df2 |>
  flextable() |>
  separate_header() |>
  autofit() |> theme_design()


#Species_Composition
taxon.fill <- merge(data.frame(group = unique(veg$landuse)), data.frame(taxon = unique(veg$taxon), Low = 0, RV = 0, High = 0)) |> mutate(type = vegnasis::fill.type(taxon)) |> unique()
taxon.fill <- taxon.fill[,c('group','taxon', 'type', 'Low', 'RV', 'High')]

veg.comp.summary <-  veg  |> summary.ESIS(group='landuse', breaks = c(5), normalize = F,
                                          lowerQ = 0, upperQ = 1) |> ungroup()
veg.comp.summary <- veg.comp.summary |> mutate(Low = cover.Low, RV=cover.mean, High=cover.High)
overstory <- veg.comp.summary |> subset(Top > 5, select = c("group","taxon", "type","Low","RV","High"))
#add missing rows
o2 <- subset(taxon.fill, taxon %in% overstory$taxon)
o2 <- subset(o2, !paste(taxon,group) %in% paste(overstory$taxon,overstory$group) )
overstory <- overstory |> rbind(o2)


allplots <- overstory |> group_by(taxon, type) |> summarise(group = "All Landuses", Low = min(Low), RV = mean(RV), High = max(High)) |> arrange(-RV )
factorgroup <- unique(overstory$group)
factortaxon <- allplots$taxon
overstory <- rbind(overstory, allplots)
overstory$taxon <- factor(overstory$taxon, levels = factortaxon)
overstory$group <- factor(overstory$group, levels = c(factorgroup,"All Landuses"))
overstory <- overstory |> arrange(group, taxon)

overstory |>
  knitr::kable(row.names = FALSE, digits = c(1,1,1,1,1)) %>%
  remove_column(1) |> column_spec(1,italic=T) |>
  kableExtra::group_rows(index = table(overstory$group)) |>
  kable_classic(full_width = F, html_font = "Cambria")


#Flextable ----

overstory.wide <- overstory  |> mutate(Low = condRound1(Low), RV = condRound1(RV), High = condRound1(High)) |> tidyr::pivot_wider(names_from = group, values_from = c(Low,RV,High)) |> as.data.frame()

ngroups <- length(unique(overstory$group))
pcolors <- palette.colors(ngroups+1, palette = "ggplot2")
pcolors <- pcolors[2:length(pcolors)]
pcolors <- c('white','white',pcolors,pcolors,pcolors)
thcols <- 1:ngroups
thcols <- 3*thcols+2
df2 <- overstory.wide

repnames <- colnames(df2)
repnames <- stringr::str_split_fixed(repnames, '_', 2)
repnamesA <- repnames[,2]
repnamesB <- repnames[,1]
repdf <- data.frame(A=repnamesA, B=repnamesB, C=pcolors)
repdf <- repdf |> mutate(seq = 1:nrow(repdf),
                         seq2 = c(1:2,(1:ngroups)+2,(1:ngroups)+2,(1:ngroups)+2))
repdf <- repdf |> arrange(seq2)
repnames <- paste0(repnames[,2],'_',repnames[,1])
colnames(df2) <- repnames
df2 <- df2[,repdf$seq]
colnames(df2)[1:2]<- c('Taxon','Habit')

theme_design <- function(x) {
  x <- border_remove(x)
  std_border <- fp_border_default(width = 0.5, color = "black")
  thk_border <- fp_border_default(width = 2, color = "black")
  x <- fontsize(x, size = 10, part = "all")
  x <- font(x, fontname = "Cambria", part = "all")
  x <- italic(x, j=1, part = "body")
  x <- align(x, align = "center", part = "all")
  x <- align(x, align = "center", part = "header")
  x <- align(x, align = "left", part = "body", j=1)
  x <- align(x, align = "center", part = "body", j=2)
  x <- bold(x, bold = TRUE, part = "all")
  x <- bg(x, bg = "white", part = "body")
  x <- bg(x, bg = repdf$C, part = "header")
  x <- bg(x, bg = "white", part = "footer")
  x <- color(x, color = "black", part = "all")
  x <- padding(x, padding = 1, part = "all")
  x <- border_outer(x, part="all", border = thk_border )
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
  x <- vline(x, j = c(2,thcols), border = thk_border, part = "all")
  x <- set_table_properties(x, layout = "fixed")
  x
}
df2 |>
  flextable() |>
  separate_header() |>
  autofit() |> theme_design()


#understory ----
understory <- veg.comp.summary |> subset(Top <= 5, select = c("group","taxon", "type", "Low","RV","High"))
u2 <- subset(taxon.fill, taxon %in% understory$taxon)
u2 <- subset(u2, !paste(taxon,group) %in% paste(understory$taxon,understory$group))
understory <- understory |> rbind(u2)

allplots <- understory |> group_by(taxon, type) |> summarise(group = "All Landuses", Low = min(Low), RV = mean(RV), High = max(High)) |> arrange(-RV )
keeptaxa <- subset(allplots, High >= 10)$taxon
factorgroup <- unique(understory$group)
factortaxon <- allplots$taxon
understory <- rbind(understory, allplots)
understory$taxon <- factor(understory$taxon, levels = factortaxon)
understory$group <- factor(understory$group, levels = c(factorgroup,"All Landuses"))
understory <- understory |> arrange(group, taxon) |> subset(taxon %in% keeptaxa)

options(knitr.kable.NA = '-')
understory |> #mutate(Low = condRound1(Low), RV = condRound1(RV), High = condRound1(High)) |>
  knitr::kable(row.names = FALSE, digits = c(1,1,1,1,1)) %>%
  remove_column(1) |> column_spec(1,italic=T) |>
  kableExtra::group_rows(index = table(understory$group)) |>
  kable_classic(full_width = F, html_font = "Cambria")



#understory flextable ----
understory.wide <- understory  |> mutate(Low = condRound1(Low), RV = condRound1(RV), High = condRound1(High)) |> tidyr::pivot_wider(names_from = group, values_from = c(Low,RV,High)) |> as.data.frame()

ngroups <- length(unique(understory$group))
pcolors <- palette.colors(ngroups+1, palette = "ggplot2")
pcolors <- pcolors[2:length(pcolors)]
pcolors <- c('white','white',pcolors,pcolors,pcolors)
thcols <- 1:ngroups
thcols <- 3*thcols+2
df2 <- understory.wide

repnames <- colnames(df2)
repnames <- stringr::str_split_fixed(repnames, '_', 2)
repnamesA <- repnames[,2]
repnamesB <- repnames[,1]
repdf <- data.frame(A=repnamesA, B=repnamesB, C=pcolors)
repdf <- repdf |> mutate(seq = 1:nrow(repdf),
                         seq2 = c(1:2,(1:ngroups)+2,(1:ngroups)+2,(1:ngroups)+2))
repdf <- repdf |> arrange(seq2)
repnames <- paste0(repnames[,2],'_',repnames[,1])
colnames(df2) <- repnames
df2 <- df2[,repdf$seq]
colnames(df2)[1:2]<- c('Taxon','Habit')

theme_design <- function(x) {
  x <- border_remove(x)
  std_border <- fp_border_default(width = 0.5, color = "black")
  thk_border <- fp_border_default(width = 2, color = "black")
  x <- fontsize(x, size = 10, part = "all")
  x <- font(x, fontname = "Cambria", part = "all")
  x <- italic(x, j=1, part = "body")
  x <- align(x, align = "center", part = "all")
  x <- align(x, align = "center", part = "header")
  x <- align(x, align = "left", part = "body", j=1)
  x <- align(x, align = "center", part = "body", j=2)
  x <- bold(x, bold = TRUE, part = "all")
  x <- bg(x, bg = "white", part = "body")
  x <- bg(x, bg = repdf$C, part = "header")
  x <- bg(x, bg = "white", part = "footer")
  x <- color(x, color = "black", part = "all")
  x <- padding(x, padding = 1, part = "all")
  x <- border_outer(x, part="all", border = thk_border )
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
  x <- vline(x, j = c(2,thcols), border = thk_border, part = "all")
  x <- set_table_properties(x, layout = "fixed")
  x
}
df2 |>
  flextable() |>
  separate_header() |>
  autofit() |> theme_design()
