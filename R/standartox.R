#' Download Standartox Data Tables from Zenodo.org
#' 
#' Downloads the Standartox data tables from Zenodo.org and reads them into R. Specific data_types can be specified.
#' 
#' @param data_type character; Specify the type of data to download. Can be one of NULL (default, downloads and imports all), "meta.fst", "phch.fst", "refs.fst", "test_fin.fst", "taxa.fst", etc.
#' @param dir_out character; Directory to which the downloaded files should be saved. Default is a temporary directory.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald \email{hannes.reinwald@@bayer.com}
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' stx_download()
#' }
#' @noRd
stx_download = function(data_type = NULL, dir_out = file.path(tempdir(),"standartox") ) {
  # Check if the output directory exists, if not create it
  if (!dir.exists(dir_out)) { dir.create(dir_out, recursive = TRUE) }
  
  # HACK this has to be done, because doi.org is the only permanent link between versions
  qurl_permanent = 'https://doi.org/10.5281/zenodo.3785030'
  req = httr::GET(qurl_permanent)
  cont = httr::content(req, as = 'text')
  # extract all zenodo download links from the content
  qurl = unique(stringr::str_extract_all(cont, 'https://zenodo.org/records/[0-9]+/files/.+')[[1]])
  qurl = grep("[.]rds|[.]fst", qurl, value = TRUE) # filter for .rsd and .fst files only! 
  
  # Filter for specific data_type if provided
  if(!is.null(data_type)){
    # could be one of: "meta.fst","phch.fst","refs.fst","test_fin.fst","taxa.fst", ...
    regx_str = paste0("/files/", sub("[.]fst$","[.]fst", sub("[.]rds$","[.]rds", data_type)) )
    regx_str = paste(regx_str, collapse = "|")
    qurl = grep(regx_str, qurl, value = TRUE)
  }
  
  # For each link in qurl check if destination file exists and if not download it.
  stxDb_ls = list() # output list 
  for(k in qurl){
    URL = sub('\">','', k)
    n   = sub("^.+/files/","", URL)
    message('\nChecking standartox file: ', n)
    
    # Define destination file path
    destfile = file.path(dir_out, n)
    
    # Check if the file already exists
    if ( !file.exists(destfile) ) {
      message('Downloading standartox ',n,' ...')
      curl::curl_download(url = URL,
                          destfile = destfile,
                          quiet = TRUE)
      message('Done! Downloaded to:\n', destfile)
    }
    else { message('File ', n, ' already exists, skipping download.') }
    
    message('Reading in file:\t', n)
    # Read in the downloaded files based on their extension 
    # for .fst: fst::read_fst(); for .rds: readRDS()
    sfx = sub("^.+[.]","", basename(destfile))
    if( sfx == "fst" ) { 
      stxDb_ls[[n]] = try ( fst::read_fst(destfile) )
      message("Done!\n")
    } else if (sfx == "rds") { 
      stxDb_ls[[n]] = readRDS(destfile)
      message("Done!\n")
    } else { warning("Unknown file format: ", sfx, "Expecting .fst or .rds files.\n") }
  }
  # Return the list of data frames
  return(stxDb_ls)
}


#' Retrieve data catalog
#' 
#' Retrieve a data catalog for all variables (and their values) that can be retrieved with stx_query()
#' 
#' @param silent logical; If TRUE, suppresses messages. Default is TRUE.
#' @param ... character; Option to specify further stx_download() parameters, e.g. dir_out. Useful if you wish to keep the files permanently on your local system by storing standartox data in a specific directory and not under tempdir().
#' 
#' @return Returns a list of data.frames containing information on data base variables
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald \email{hannes.reinwald@@bayer.com}
#' 
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' ls = stx_catalog()
#' 
#' # to get verbose output from the function
#' ls = stx_catalog(silent = FALSE)
#' 
#' # to specify a directory to which the catalog should be downloaded
#' ls = stx_catalog(silent = FALSE, dir_out = "~/tmp")
#' # This will create a directory under ~/tmp and download the catalog.rds file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' @export
stx_catalog = function(silent = TRUE, ...) {
  if (!silent) message('Retrieving Standartox catalog ...')
  if (silent) {
    result = suppressMessages( stx_download(data_type = 'catalog.rds', ...)[[1]] )
  } else {
    result = stx_download(data_type = 'catalog.rds', ...)[[1]]
  }
  return(result)
}


#' Retrieve Standartox toxicity values
#'
#' Retrieve toxicity values from the Standartox data base \url{http://standartox.uni-landau.de/}.
#'
#' @import httr jsonlite fst data.table
#'
#' @param cas character, integer; Limit data base query to specific CAS numbers, multiple entries possible (e.g. 1071-83-6, 1071836), NULL (default).
#' @param concentration_unit character; Limit data base query to specific concentration units (e.g. ug/l - default).
#' @param concentration_type character; Limit data base query to specific concentration types, can be one of NULL (default), 'active ingredient', 'formulation', 'total', 'not reported', 'unionized', 'dissolved', 'labile'. See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.4.
#' @param duration integer vector of length two; Limit data base query to specific test durations (hours) (e.g. c(24, 48)). NULL (default).
#' @param endpoint character; Choose endypoint type, must be one of 'XX50' (default), 'NOEX', 'LOEX'.
#' @param effect character; Limit data base query to specific effect groups, multiple entries possible (e.g. 'Mortality', 'Intoxication', 'Growth'). See \url{https://cfpub.epa.gov/ecotox/pdf/codeappendix.pdf} p.95. NULL (default).
#' @param exposure character; Choose exposure type, (e.g. aquatic, environmental, diet). NULL (default).
#' @param chemical_role character; Limit data base query to specific chemical roles (e.g. insecticide), multiple entries possible. NULL (default).
#' @param chemical_class character; Limit data base query to specific chemical classes (e.g. neonicotinoid), multiple entries possible. NULL (default).
#' @param taxa character; Limit data base query to specific taxa, multiple entries possible. NULL (default).
#' @param ecotox_grp character; Convenience grouping of organisms in ecotoxicology, must be one of NULL (default), 'invertebrate', 'fish', 'plant_land', 'macrophyte', 'algae'.
#' @param trophic_lvl character; Trophic level of organism, must be one of NULL (default), 'autotroph', 'heterotroph'.
#' @param habitat character; Limit data base query to specific organism habitats, can be one of NULL (default) 'marine', 'brackish', 'freshwater', 'terrestrial'.
#' @param region character; Limit data base query to organisms occurring in specific regions, can be one of NULL (default) 'africa', 'america_north', 'america_south', 'asia', 'europe', 'oceania'.
#' @param vers integer; Choose the version of the EPA Ecotox on which Standartox is based on. NULL (default) accesses the most recent version.
#' @param ... currently not used
#'
#' @return Returns a list of three data.tables (filtered data base query results, aggregated data base query results, meta information)
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
#' @examples
#' \donttest{
#' # might fail if API is not available
#' stx_query('1071-83-6')
#' stx_query(cas = '1071-83-6',
#'           duration = c(48, 120),
#'           concentration_unit = 'ug/l')
#' stx_query(cas = '1071-83-6',
#'           duration = c(48, 120),
#'           concentration_unit = 'ug/l',
#'           endpoint = 'XX50')
#' }
#' 
#' @export
stx_query = function(cas = NULL,
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
                     vers = NULL,
                     ...) {
  # to avoid NOTE in R CMD check --as-cran
  casnr = outlier = concentration = cname = NULL
  # debuging
  # browser() # debuging
  # cas = '1071-83-6'; concentration_unit = NULL; concentration_type = NULL; duration = NULL; endpoint = 'XX50'; effect = NULL; exposure = NULL; chemical_role = NULL; chemical_class = NULL; taxa = NULL; ecotox_grp = NULL; trophic_lvl = NULL; habitat = NULL; region = NULL; vers = NULL
  # checks
  endpoint = match.arg(endpoint)
  # request
  body = list(casnr = cas,
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
              region = region,
              vers = vers)
  # POST
  stx_message(body)
  res = try(httr::POST(
    file.path(domain(), 'filter'),
    body = body,
    encode = 'json',
  ), silent = TRUE)
  stx_availability(res)
  if (res$status_code == 400) {
    warning(jsonlite::fromJSON(httr::content(res, type = 'text', encoding = 'UTF-8')))
    out_fil = data.table(NA)
    filtered = data.table(NA)
    out_agg = data.table(NA)
  }
  if (res$status_code == 200) {
    out_fil = read_bin_vec(res$content, type = 'fst')
    if (nrow(out_fil) == 0) {
      warning('No results found.')
      out_fil = data.table(NA)
      filtered = data.table(NA)
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
       # TODO id = id,
       aggregated = out_agg,
       meta = out_meta)
}


#' Retrieve chemical data
#' 
#' Retrieve data on all chemicals in Standartox.
#' 
#' @return Returns a data.table containing informaiton on chemicals in Standartox.
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald \email{hannes.reinwald@@bayer.com}
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' df = stx_chem()
#' 
#' # to get verbose output from the function
#' df = stx_chem(silent = FALSE)
#' 
#' # to specify a directory to which the catalog should be downloaded
#' df = stx_chem(silent = FALSE, dir_out = "~/tmp")
#' # This will create a directory under ~/tmp and download the respective standartox file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' 
#' @export
stx_chem = function(silent = TRUE, ...) {
  if (!silent) message('Retrieving Standartox listed Chemicals ...')
  if (silent) {
    result = suppressMessages( stx_download(data_type = 'phch.fst', ...)[[1]] )
  } else {
    result = stx_download(data_type = 'phch.fst', ...)[[1]]
  }
  return(result)
}


#' Retrieve taxa data
#' 
#' Retrieve data on all taxa in Standartox.
#' 
#' @return Returns a data.table containing informaiton on taxa in Standartox.
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald \email{hannes.reinwald@@bayer.com}
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' df = stx_taxa()
#' 
#' # to get verbose output from the function
#' df = stx_taxa(silent = FALSE)
#' 
#' # to specify a directory to which the catalog should be downloaded
#' df = stx_taxa(silent = FALSE, dir_out = "~/tmp")
#' # This will create a directory under ~/tmp and download the respective standartox file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' 
#' @export
stx_taxa = function(silent = TRUE, ...) {
  if (!silent) message('Retrieving Standartox listed Taxa ...')
  if (silent) {
    result = suppressMessages( stx_download(data_type = 'taxa.fst', ...)[[1]] )
  } else {
    result = stx_download(data_type = 'taxa.fst', ...)[[1]]
  }
  return(result)
}


# IDEA
# microbenchmark::microbenchmark({
# col_test = c('result_id', 'cas', 'species_number', 'ref_number',
#              'concentration', 'concentration_unit', 'concentration_orig', 'concentration_unit_orig',
#              'concentration_type', 'duration', 'duration_unit',
#              'endpoint', 'effect', 'exposure', 'outlier')
# col_chem = c('cname', 'iupac_name', 'cas', 'cas', 'inchikey', 'inchi',
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
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @noRd
#'  
stx_aggregate = function(dat = NULL) {
  # assign variables to avoid R CMD check NOTES
  . = concentration = cname = cas = tax_taxon = gmn = gmnsd = n = NULL
  # checking
  if (is.null(dat)) stop('Provide table.')
  # aggregation
  dat[
    ,
    .(gmn = gm_mean(concentration),
      gmnsd = gm_sd(concentration),
      n = .N),
    .(cname, cas, tax_taxon) 
    ][
      ,
      .(min = min(gmn),
        tax_min = .SD[ which.min(gmn), tax_taxon ],
        gmn = gm_mean(gmn),
        gmnsd = gm_sd(gmnsd),
        max = max(gmn),
        tax_max = .SD[ which.max(gmn), tax_taxon ],
        n = sum(n),
        tax_all = paste0(sort(unique(tax_taxon)), collapse = ', ')),
      .(cname, cas)
  ]
}

#' Connection URL
#' 
#' @author Andreas Scharmüller
#' @noRd
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
#' @author Andreas Scharmüller
#' @noRd
#' 
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

#' Check availability of connection
#' 
#' @author Andreas Scharmüller
#' @noRd
#' 
stx_availability = function(res,
                            http_codes = c(200, 400)) {
  if (inherits(res, 'try-error') ||
      ! httr::status_code(res) %in% http_codes) {
    msg = paste0(
    'The standartox web service seems currently unavailable.
    Please try again after some time. Should it still not work then, please file an issue here:
https://github.com/andschar/standartox/issues
Error code: ', res)
    stop(msg)
  }
}
