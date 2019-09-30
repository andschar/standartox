#' connection
#' 
#' @keywords internal
#' 
domain = function() {
  baseurl = '139.14.20.252'
  port = 8000
  domain = paste0(baseurl, ':', port)
  
  return(domain)
}

#' Retrieve meta data
#' 
#' @keywords internal
stx_meta = function(vers = NULL) {
  # request
  qurl = file.path(domain(), 'meta')
  body = list(vers = vers)
  # POST
  res = httr::POST(qurl,
                   body = body,
                   encode = 'json')
  cont = httr::content(res, type = 'text', encoding = 'UTF-8')
  out = jsonlite::fromJSON(cont)
  # standartox verison
  standartox_vers = data.frame(variable = 'standartox_version',
                               value = grep('Version', readLines('DESCRIPTION'), value = TRUE))
  # resturn
  out = do.call(rbind, list(out, standartox_vers))
  
  return(out)
}

#' Retrieve data catalog
#' 
#' Retrieve a data catalog for all variables (and their values) that can be retrieved with stx_query()
#' 
#' @return Returns a list of data.frames containing information on data base variables
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @examples
#' \donttest{
#' # might fail if API is not available
#' stx_catalog()
#' }
#' 
#' @export
stx_catalog = function(vers = NULL) {
  # request
  qurl = file.path(domain(), 'catalog')
  body = list(vers = vers)
  # POST
  res = httr::POST(qurl,
                   body = body,
                   encode = 'json',
                   httr::verbose())
  cont = httr::content(res, type = 'text', encoding = 'UTF-8')
  out = jsonlite::fromJSON(cont)
  
  return(out)
}

#' Retrieve Standartox toxicity values
#'
#' Retrieve toxicity values from the Standartox data base \url{http://standartox.uni-landau.de/}.
#'
#' @import data.table
#'
#' @param vers integer; Choose the version of the EPA Ecotox on which Standartox is based on. NULL (default) accesses the most recent version
#' @param cas character, integer; Limit data base query to specific CAS numbers, multiple entries possible (e.g. 1071-83-6, 1071836), NULL (default)
#' @param concentration_type character; Limit data base query to specific concentration types, can be one of NULL (default), 'active ingredient', 'formulation', 'total', 'not reported', 'unionized', 'dissolved', 'labile'. See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.4
#' @param chemical_class character; Limit data base query to specific chemical classes, multiple entries possible, NULL (default)
#' @param taxa character; Limit data base query to specific taxa, multiple entries possible, NULL (default)
#' @param habitat character; Limit data base query to specific organism habitats, can be one of NULL (default) 'marine', 'brackish', 'freshwater'
#' @param region character; Limit data base query to organisms occurring in specific regions, can be one of NULL (default) 'africa', 'america_north', 'america_south', 'asia', 'europe', 'oceania'
#' @param duration integer vector of length two; Limit data base query to specific test durations (hours) (e.g. c(24, 48))
#' @param effect character; Limit data base query to specific effect groups, multiple entries possible (e.g. 'MOR', 'ITX', 'GRO'). See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.95
#' @param endpoint character; Choose endypoint type, must be one of 'XX50' (default), 'NOEX', 'LOEX'
#' @param aggregate character; Choose aggregation method, can be one of 'min', 'med', 'gmn' (default), 'max'
#' @param ... currently not used
#'
#' @return Returns a list of three data.tables (filtered data base query results, aggregated data base query results, meta information)
#' @author Andreas Scharmueller \email{andschar@@proton.com}
#' 
#' @examples
#' \donttest{
#' # might fail if API is not available
#' stx_query('1071-83-6')
#' stx_query('1071-83-6', duration = c(48, 120))
#' stx_query('1071-83-6', duration = c(48, 120), endpoint = 'XX50', aggregate = 'md')
#' }
#' 
#' @export
stx_query = function(vers = NULL,
                     cas = NULL,
                     concentration_type = NULL,
                     chemical_class = NULL,
                     taxa = NULL,
                     habitat = NULL,
                     region = NULL,
                     duration = NULL,
                     effect = NULL,
                     endpoint = c('XX50', 'NOEX', 'LOEX'),
                     agg = c('min', 'med', 'gmn', 'max'),
                     ...) {
  
  # read binary vector function
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

    return(res)
  }
  endpoint = match.arg(endpoint)
  agg = match.arg(agg, several.ok = TRUE)
  # request
  qurl = file.path(domain(), 'filter', 'rds')
  body = list(vers = vers,
              cas = cas,
              conc1_type = concentration_type,
              chemical_class = chemical_class,
              taxa = taxa,
              habitat = habitat,
              region = region,
              duration = duration,
              effect = effect,
              endpoint = endpoint)
  # POST
  res = httr::POST(qurl,
                   body = body,
                   encode = 'json',
                   httr::verbose())
  if (res$status_code != 200)
    stop(res$status_code)
  # data
  out_fil = read_bin_vec(res$content, type = 'fst')
  # aggregate
  qurl = file.path(domain(), 'aggregate')
  res = httr::GET(qurl) # GET function from API
  if (res$status_code != 200)
    stop(res$status_code)
  stx_aggregate = read_bin_vec(res$content, type = 'rds')
  out_agg = stx_aggregate(out_fil, agg = agg)
  # meta
  out_meta = stx_meta()
  # return
  out = list(filtered = out_fil,
             aggregated = out_agg,
             meta = out_meta)
  
  return(out)
}














