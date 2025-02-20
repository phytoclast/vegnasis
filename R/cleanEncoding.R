#' Clean character encoding text.
#' @description
#' Often when importing plain text files the character encoding is unknown and sometimes of mixed origin. This poses a problem with special characters such as foreign species author names or hybrid symbols. This crude script uses tools from the stringi package to detect non UTF-8 encoding such as Window's 'latin1' and convert it to UTF-8. At best, incorrect encoding will render nonsense characters, while at worse, it will cause functions to crash.
#'
#'
#' @param x String characters of unknown encoding
#'
#' @return String characters of UTF-8 encoding
#' @export
#'
#' @examples
#' x= '� Pachgerocereus orcuttii' # Example data correctly encoded by default, but this is how incorrect rendering may look.
#' x= cleanEncoding(x)
#' '× Pachgerocereus orcuttii' # This is how the above text should have been correctly rendered, if it were an actual data encoding error.
#'
cleanEncoding <- function(x){
  n = length(x)
  for(i in 1:n){#i=1
    en <- stringi::stri_enc_detect(x[i])[[1]]
    en <- data.frame(enc = en$Encoding, con = en$Confidence)
    en <- en |> mutate(con = ifelse(con >= 0.8 & enc %in% 'UTF-8', con*2,con)) |> subset(con %in% max(con))
    en <- en$enc[1]
    if(!en %in% 'UTF-8'){
      if(en %in% 'windows-1252'){
        x[i] <- stringi::stri_conv(x[i], from = 'windows-1252', to='UTF-8')
      }else if(en %in% 'ISO-8859-1'){
        x[i] <- stringi::stri_conv(x[i], from = 'ISO-8859-1', to='UTF-8')
      }else{
        x[i] <- stringi::stri_conv(x[i], from = 'latin1', to='UTF-8')
      }
    }
  }
  return(x)}
