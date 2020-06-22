#' Utility functions for the standartox package

#' Read binary vector function
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @noRd
#'   
read_bin_vec = function(vec, type = c('rds', 'fst')) {
  if (type == 'rds') {
    # from https://stackoverflow.com/questions/58135794/read-binary-vector/58136567#58136567
    con = gzcon(rawConnection(vec))
    res = readRDS(con)
    on.exit(close(con))
  }
  if (type == 'fst') {
    tmp = tempfile()
    writeBin(vec, tmp)
    res = fst::read_fst(tmp, as.data.table = TRUE)
  }
  res
}

#' Convert CAS betwenn hyphen and hyphenless form
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @noRd
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
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @noRd
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
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @noRd
#' 
rm_col_na = function(dt) {
  Filter(function(x) !all(is.na(x)), dt)
}

#' Calculate geometric mean
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @noRd
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

#' Calculate geometric standard deviation
#' 
#' @author Andreas Scharmüller \email{andschar@@protonmail.com}
#' @noRd
#'
gm_sd = function (x, na.rm = TRUE, sqrt.unbiased = TRUE) {
  # after EnvStats::geoSD()
  if (!is.vector(x, mode = "numeric") || is.factor(x))
    stop("'x' must be a numeric vector")
  wna = which(is.na(x))
  if (length(wna) != 0) {
    if (na.rm) 
      x = x[-wna]
    else return(NA)
  }
  if (any(x <= 0)) {
    warning("Non-positive values in 'x'")
    return(NA_real_)
  } else {
    sd.log = sd(log(x))
    if (!sqrt.unbiased) {
      n = length(x)
      sd.log = sqrt((n - 1)/n) * sd.log
    }
    exp(sd.log)
  }
}

#' Function to flag outliers
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @noRd
#' 
flag_outliers = function(x, lim = 1.5, na.rm = TRUE, ...) {
  qnt = quantile(x, probs = c(.25, .75), na.rm = na.rm)
  H = lim * IQR(x, na.rm = na.rm)
  fifelse(x < qnt[1] - H | x > qnt[2] + H, TRUE, FALSE)
}




