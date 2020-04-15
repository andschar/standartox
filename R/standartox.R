#' connection
#' 
#' @keywords internal
#' 
domain = function() {
  baseurl = 'http://139.14.20.252'
  # baseurl = 'http://127.0.0.1'
  port = 8000
  domain = paste0(baseurl, ':', port)
  
  return(domain)
}

#' Retrieve meta data
#' 
#' @keywords internal
stx_meta = function(vers = NULL) {
  # request
  body = list(vers = vers)
  # POST
  res = httr::POST(
    url = file.path(domain(), 'meta'),
    body = body,
    encode = 'json'
  )
  cont = httr::content(res, type = 'text', encoding = 'UTF-8')
  out = jsonlite::fromJSON(cont)
  
  return(out)
}

#' Retrieve data catalog
#' 
#' Retrieve a data catalog for all variables (and their values) that can be retrieved with stx_query()
#' 
#' @param vers integer; Choose the version of the EPA Ecotox on which Standartox is based on. NULL (default) accesses the most recent version
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
  body = list(vers = vers)
  # POST
  message('Retrieving Standartox catalog.')
  res = httr::POST(
    url = file.path(domain(), 'catalog'),
    body = body,
    encode = 'json',
    # httr::verbose()
  )
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
#' @param concentration_unit character; Limit data base query to specific concentration units (e.g. ug/l - default) 
#' @param concentration_type character; Limit data base query to specific concentration types, can be one of NULL (default), 'active ingredient', 'formulation', 'total', 'not reported', 'unionized', 'dissolved', 'labile'. See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.4
#' @param chemical_role character; Limit data base query to specific chemical roles (e.g. insecticide), multiple entries possible, NULL (default)
#' @param chemical_class character; Limit data base query to specific chemical classes (e.g. neonicotinoid), multiple entries possible, NULL (default)
#' @param taxa character; Limit data base query to specific taxa, multiple entries possible, NULL (default)
#' @param tropic_lvl character; Trophic level of organism, must be one of 'autotroph', 'heterotroph', NULL (default)
#' @param habitat character; Limit data base query to specific organism habitats, can be one of NULL (default) 'marine', 'brackish', 'freshwater'
#' @param region character; Limit data base query to organisms occurring in specific regions, can be one of NULL (default) 'africa', 'america_north', 'america_south', 'asia', 'europe', 'oceania'
#' @param ecotox_grp character; Convenience grouping of organisms in ecotoxicology, must be one of 'invertebrate', 'fish', 'plant_land', 'macrophyte', 'algae', NULL (default)
#' @param duration integer vector of length two; Limit data base query to specific test durations (hours) (e.g. c(24, 48))
#' @param effect character; Limit data base query to specific effect groups, multiple entries possible (e.g. 'Mortality', 'Intoxication', 'Growth'). See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.95
#' @param endpoint character; Choose endypoint type, must be one of 'XX50' (default), 'NOEX', 'LOEX'
#' @param exposure character; Choose exposure type, (e.g. aquatic, environmental, diet)
#' @param agg character; Choose aggregation method, can be one of 'min', 'gmn' (default), 'max'
#' @param ... currently not used
#'
#' @return Returns a list of three data.tables (filtered data base query results, aggregated data base query results, meta information)
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
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
#'
stx_query = function(vers = NULL,
                     cas = NULL,
                     concentration_unit = NULL,
                     concentration_type = NULL,
                     chemical_role = NULL,
                     chemical_class = NULL,
                     taxa = NULL,
                     trophic_lvl = NULL,
                     habitat = NULL,
                     region = NULL,
                     ecotox_grp = NULL,
                     duration = NULL,
                     effect = NULL,
                     endpoint = c('XX50', 'NOEX', 'LOEX'),
                     exposure = NULL,
                     agg = c('min', 'gmn', 'max'),
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
    res
  }
  # checks
  endpoint = match.arg(endpoint)
  agg = match.arg(agg, several.ok = TRUE)
  # request
  body = list(vers = vers,
              cas = cas,
              concentration_unit = concentration_unit,
              concentration_type = concentration_type,
              chemical_role = chemical_role,
              chemical_class = chemical_class,
              taxa = taxa,
              trophic_lvl = trophic_lvl,
              habitat = habitat,
              region = region,
              ecotox_grp = ecotox_grp,
              duration = duration,
              effect = effect,
              endpoint = endpoint,
              exposure = exposure)
  # browser() # debuging
  # POST
  stx_message(body)
  res = httr::POST(
    file.path(domain(), 'filter'),
    body = body,
    encode = 'json',
    # httr::verbose()
  )
  if (res$status_code == 400) {
    warning(jsonlite::fromJSON(httr::content(res, type = 'text', encoding = 'UTF-8')))
    out_fil = data.table(NA)
    out_agg = data.table(NA)
  }
  if (res$status_code != 200) {
    warning(res$status_code)
    out_fil = data.table(NA)
    out_agg = data.table(NA)
  }
  if (res$status_code == 200) {
    out_fil = read_bin_vec(res$content, type = 'fst')
    if (nrow(out_fil) == 0) {
      warning('No results found.')
      out_fil = data.table(NA)
      out_agg = data.table(NA)
    } else {
      out_fil[ , cas := cas_conv(casnr) ]
      # aggregate
      res = httr::GET(file.path(domain(), 'aggregate')) # GET function from API
      if (res$status_code != 200)
        stop(res$status_code)
      fun_l = read_bin_vec(res$content, type = 'rds') # binary list object with two funcitons
      stx_aggregate = fun_l[[1]]
      flag_outliers = fun_l[[2]]
      para = c('cas',
               'concentration_unit',
               'concentration_type',
               'duration',
               'tax_taxon',
               'effect',
               'endpoint',
               'exposure')
      out_fil[ ,
               outlier := flag_outliers(concentration),
               by = para ]
      out_agg = suppressWarnings(
        stx_aggregate(out_fil,
                      vl = 'concentration',
                      agg = agg)
      )
    }
  }
  # meta
  out_meta = stx_meta()
  # return
  out = list(filtered = out_fil,
             aggregated = out_agg,
             meta = out_meta)
  
  return(out)
}



