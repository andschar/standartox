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

#' Compose query message 
#'
#' @keywords internal
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
stx_message = function(body) {
  body2 = body[ sapply(body, function(x) !is.null(x)) ]
  body2 = lapply(body2, paste0, collapse = ', ')
  body2 = lapply(body2, function(x) {
    if (nchar(x) >= 80) {
      paste0(substr(paste0(x, collapse = ''), 1, 70), '...[truncated]')
    } else {
      x
    }
  })
  msg = paste0(paste0(names(body2), ': ', unlist(body2)),
               collapse = '\n')
  message('Standartox query running...\nParameters:\n', msg)
}

