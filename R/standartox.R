#' connection
#' 
#' @keywords internal
#' 
domain = function() {
  baseurl = 'http://139.14.20.252'
  # baseurl = 'http://127.0.0.1' # debuging
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
  message('Retrieving Standartox catalog..')
  res = httr::POST(
    url = file.path(domain(), 'catalog'),
    body = body,
    encode = 'json',
    # httr::verbose()
  )
  cont = httr::content(res, type = 'text', encoding = 'UTF-8')
  out = jsonlite::fromJSON(cont)
  
  out
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
#' @param duration integer vector of length two; Limit data base query to specific test durations (hours) (e.g. c(24, 48))
#' @param endpoint character; Choose endypoint type, must be one of 'XX50' (default), 'NOEX', 'LOEX'
#' @param effect character; Limit data base query to specific effect groups, multiple entries possible (e.g. 'Mortality', 'Intoxication', 'Growth'). See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.95
#' @param exposure character; Choose exposure type, (e.g. aquatic, environmental, diet)
#' @param chemical_role character; Limit data base query to specific chemical roles (e.g. insecticide), multiple entries possible, NULL (default)
#' @param chemical_class character; Limit data base query to specific chemical classes (e.g. neonicotinoid), multiple entries possible, NULL (default)
#' @param taxa character; Limit data base query to specific taxa, multiple entries possible, NULL (default)
#' @param ecotox_grp character; Convenience grouping of organisms in ecotoxicology, must be one of 'invertebrate', 'fish', 'plant_land', 'macrophyte', 'algae', NULL (default)
#' @param tropic_lvl character; Trophic level of organism, must be one of 'autotroph', 'heterotroph', NULL (default)
#' @param habitat character; Limit data base query to specific organism habitats, can be one of NULL (default) 'marine', 'brackish', 'freshwater'
#' @param region character; Limit data base query to organisms occurring in specific regions, can be one of NULL (default) 'africa', 'america_north', 'america_south', 'asia', 'europe', 'oceania'
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
#' stx_query('1071-83-6', duration = c(48, 120), endpoint = 'XX50')
#' }
#' 
#' @export
#'
stx_query = function(vers = NULL,
                     casnr = NULL,
                     concentration_unit = NULL,
                     concentration_type = NULL,
                     duration = NULL,
                     endpoint = c('XX50', 'NOEX', 'LOEX'),
                     effect = NULL,
                     exposure = NULL,
                     chemical_role = NULL,
                     chemical_class = NULL,
                     taxa = NULL,
                     ecotox_grp = NULL,
                     trophic_lvl = NULL,
                     habitat = NULL,
                     region = NULL,
                     ...) {
  # browser() # debuging
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
  # request
  body = list(vers = vers,
              casnr = casnr,
              concentration_unit = concentration_unit,
              concentration_type = concentration_type,
              duration = duration,
              endpoint = endpoint,
              effect = effect,
              exposure = exposure,
              chemical_role = chemical_role,
              chemical_class = chemical_class,
              taxa = taxa,
              ecotox_grp = ecotox_grp,
              trophic_lvl = trophic_lvl,
              habitat = habitat,
              region = region)
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
      # CAS column (not sent through API to reduce size)
      out_fil[ , cas := cas_conv(casnr) ][ , casnr := NULL ]
      # outliers
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
      # colorder
      nam = names(out_fil)
      col_order = c('cname', 'cas', 'iupac_name', 'inchikey', 'inchi', 'molecularweight',
                    'result_id', 'endpoint', 'effect', 'exposure', 'trophic_lvl', 'ecotox_grp', 'concentration_type',
                    'concentration', 'concentration_unit', 'concentration_orig', 'concentration_unit_orig',
                    'duration', 'duration_unit', 'outlier',
                    c('species_number', grep('tax_|hab_|reg_', nam, value = TRUE)),
                    grep('cro_|ccl_', nam, value = TRUE),
                    grep('ref', nam, value = TRUE))
      setcolorder(out_fil, col_order)
      # order
      setorder(out_fil, cname)
      # id
      col_id = c('cname', 'cas', 'inchikey', 'inchi', 'result_id', 'species_number', 'ref_number')
      id = out_fil[ , .SD, .SDcols = col_id ]
      # short
      col_short = c('cname', 'cas', 'inchikey',
                    'endpoint', 'effect', 'exposure', 'trophic_lvl', 'ecotox_grp', 'concentration_type',
                    'concentration', 'concentration_unit', 'concentration_orig', 'concentration_unit_orig',
                    'duration', 'duration_unit', 'outlier',
                    grep('tax_', nam, value = TRUE))
      filtered = out_fil[ , .SD, .SDcols = col_short ]
      # aggregate
      out_agg = suppressWarnings(
        stx_aggregate(out_fil)
      )
    }
  }
  # meta
  out_meta = stx_meta()
  # return
  list(filtered = filtered,
       filtered_all = rm_col_na(out_fil),
       id = id,
       aggregated = out_agg,
       meta = out_meta)
}

# IDEA
# microbenchmark::microbenchmark({
# col_test = c('result_id', 'casnr', 'species_number', 'ref_number',
#              'concentration', 'concentration_unit', 'concentration_orig', 'concentration_unit_orig',
#              'concentration_type', 'duration', 'duration_unit',
#              'endpoint', 'effect', 'exposure', 'outlier')
# col_chem = c('cname', 'iupac_name', 'cas', 'casnr', 'inchikey', 'inchi',
#              'molecularweight',
#              grep('cro|ccl', names(out_fil), value = TRUE))
# col_taxa = c(
#   grep('tax|hab|reg', names(out_fil), value = TRUE))
# col_refs = grep('ref', names(out_fil), value = TRUE)
# l = list(
#   test = unique(rm_col_na(out_fil[ , .SD, .SDcols = col_test ])),
#   chem = unique(rm_col_na(out_fil[ , .SD, .SDcols = col_chem ])),
#   taxa = unique(rm_col_na(out_fil[ , .SD, .SDcols = col_taxa ])),
#   refs = unique(rm_col_na(out_fil[ , .SD, .SDcols = col_refs ])),
#   aggregated = out_agg,
#   meta = stx_meta()
# )
# })
# 


#' Function to aggregate filtered test results
#' 
#' @keywords internal
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
stx_aggregate = function(dat = NULL) {
  # checking
  if (is.null(dat)) stop('Provide table.')
  # aggregation
  out = dat[
    ,
    .(gmn = gm_mean(concentration),
      n = .N),
    .(cname, cas, tax_taxon) 
    ][
      ,
      .(min = min(gmn),
        tax_min = .SD[ which.min(gmn), tax_taxon ],
        gmn = gm_mean(gmn),
        amn = mean(gmn),
        sd = sd(gmn),
        max = max(gmn),
        tax_max = .SD[ which.max(gmn), tax_taxon ],
        n = sum(n),
        tax_all = paste0(sort(unique(tax_taxon)), collapse = ', ')),
      .(cname, cas)
  ]
  
  out
}


