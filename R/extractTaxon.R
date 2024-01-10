#extract Taxon

#' Extract Taxon
#' @description
#' This function takes a full taxonomic name complete with authors and infrataxa (variety and subspecies and forms) and parses out the different elements. Hybrids indicated by the letter 'X' are converted to the multiply '×' symbol and spaces are removed so that hybrid species can be treated with the same number of word elements as non hybrids.
#'
#' @param rawnames Vector of taxonomic names with or without authors and infrataxa.
#' @param report What element of taxon name to report ("taxon", "genus", "binomial" (genus and specific epithet only), "epithet", "infrataxon" (variety/subspecies epithet), "author").
#'
#' @return Name of taxon or author depending on report parameter ("taxon" = full taxon without author; "binomial" = genus and specific epithet; "author" = author of of lowest infrataxon for which author name is provided.)
#' @export
#'
#' @examples rawnames = c('Arnica angustifolia Vahl subsp. tomentosa (Macoun) G.W. Douglas & G. Ruyle-Douglas', 'Abies ×shastensis (Lemmon) Lemmon', 'Abies X shastensis (Lemmon) Lemmon','Abies balsamea ssp. lasiocarpa', 'Abies balsamea (L.) Mill. var. fallax (Engelm.) B. Boivin','Agastache pallidiflora (A. Heller) Rydb. ssp. pallidiflora var. greenei (Briq.) R.W. Sanders')
#' @examples extractTaxon(rawnames,'taxon')
#' @examples extractTaxon(rawnames,'binomial')
#' @examples extractTaxon(rawnames,'author')
#'
extractTaxon <- function(rawnames, report = 'taxon'){
  x = data.frame(rawname = rawnames)
  x <- x |> mutate(rawname = str_replace_all(rawname,'^×\\s|^x\\s|^X\\s','×'),
                   rawname = str_replace_all(rawname,'\\s×\\s|\\sx\\s|\\sX\\s',' ×'),
                   genus = trimws(str_split_fixed(rawname,'[[:space:]]',2)[,1]),
                   first = trimws(str_split_fixed(rawname,'[[:space:]]',2)[,2]),
                   second = ifelse(grepl('^[a-z]|^×',first) & !grepl('^ex[[:space:]]',first),
                                   trimws(str_split_fixed(first,'[[:space:]]',2)[,1]),""),
                   xxx = ifelse(grepl('^[a-z]|^×',first) & !grepl('^ex[[:space:]]',first),
                                trimws(str_split_fixed(first,'[[:space:]]',2)[,2]),first),
                   xxx = str_replace_all(xxx,"\\sssp\\.\\s|^ssp\\.\\s|\\ssubsp.\\s|^subsp\\.\\s|\\snothossp\\.\\s|\\snothosubsp\\.\\s|\\ssubspecies\\s|^nothossp\\.\\s|^nothosubsp\\.\\s|^subspecies\\s", '_ssp._' ),
                   xxx = str_replace_all(xxx, "\\svar\\.\\s|^var\\.\\s|\\snothovar\\.\\s|\\svariety\\s|^nothovar\\.\\s|^variety\\s", '_var._' ),
                   xxx = ifelse(grepl('\\sf.\\s[a-z]|^f.\\s[a-z]',xxx) & !grepl('\\sf.\\sex\\s',xxx),
                                str_replace_all(xxx, "\\sf\\.\\s|^f\\.\\s", '_f._' ),xxx),
                   xxx = str_replace_all(xxx,"^forma\\s|^form\\s|^nothof\\.\\s|\\sforma\\s|\\sform\\s|\\snothof\\.\\s", '_f._' ),
                   xxx1 = trimws(str_split_fixed(xxx,'_',3)[,1]),
                   xxx2 = trimws(str_split_fixed(xxx,'_',3)[,2]),
                   xxx3 = trimws(str_split_fixed(xxx,'_',4)[,3]),
                   xxx4 = trimws(str_split_fixed(xxx,'_',5)[,4]),
                   xxx5 = trimws(str_split_fixed(xxx,'_',6)[,5]),
                   epithet = second,
                   binomialauthor = xxx1,
                   infrarank1 = xxx2,
                   infraname1 = trimws(str_split_fixed(xxx3,'[[:space:]]',2)[,1]),
                   infraauthor1 = trimws(str_split_fixed(xxx3,'[[:space:]]',2)[,2]),
                   infrarank2 = xxx4,
                   infraname2 = trimws(str_split_fixed(xxx5,'[[:space:]]',2)[,1]),
                   infraauthor2 = trimws(str_split_fixed(xxx5,'[[:space:]]',2)[,2]),
                   # first=NULL,
                   # second=NULL,
                   # xxx=NULL,
                   # xxx1=NULL,
                   # xxx2=NULL,
                   # xxx3=NULL,
                   # xxx4=NULL,
                   # xxx5=NULL,
                   binomial = trimws(paste(genus, epithet)),
                   taxon = trimws(paste(binomial, infrarank1, infraname1, infrarank2, infraname2)),
                   infrarank = trimws(ifelse(!infrarank2 %in% "", infrarank2, infrarank1)),
                   infrataxon = trimws(ifelse(!infraname2 %in% "", infraname2, infraname1)),
                   author = trimws(ifelse(!infraauthor2 %in% "", infraauthor2, ifelse(!infraauthor1 %in% "", infraauthor1, binomialauthor)))
  )
  if(report == 'author'){return(x$author)}else if(report=='binomial'){return(x$binomial)}else if(report == 'genus'){return(x$genus)}else if(report == 'infrarank'){return(x$infrarank)}else if(report == 'infrataxon'){return(x$infrataxon)}else if(report == 'epithet'){return(x$epithet)}else if(report == 'all'){return(x[,c('genus', 'epithet', 'infrarank', 'infrataxon')])}else{return(x$taxon)}
}
