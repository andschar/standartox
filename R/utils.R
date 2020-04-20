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
  message('Standartox query running..\nParameters:\n', msg)
}

#' Filter all-NA columns
#' 
#' @keywords internal
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
rm_col_na = function(dt) {
  Filter(function(x) !all(is.na(x)), dt)
}

#' Calculate geometric mean
#' 
#' @keywords internal
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
# function to calculate the geometric mean
# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm = TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
  }
}

#' Function to flag outliers
#' 
#' @keywords internal
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
flag_outliers = function(x, lim = 1.5, na.rm = TRUE, ...) {
  qnt = quantile(x, probs = c(.25, .75), na.rm = na.rm)
  H = lim * IQR(x, na.rm = na.rm)
  fifelse(x < qnt[1] - H | x > qnt[2], TRUE, FALSE)
}




