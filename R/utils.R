#' Utility functions for the standartox package

#' Convert CAS betwenn hyphen and hyphenless form
#'
#' @keywords internal
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#'  
cas_conv = function(cas) {
  if (any(grepl('-', cas))) {
    gsub('-', '', cas)
  } else {
    paste(substr(cas, 1, nchar(cas)-3),
          substr(cas, nchar(cas)-2, nchar(cas)-1),
          substr(cas, nchar(cas), nchar(cas)),
          sep = '-')  
  }
}
