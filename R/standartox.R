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
#' stxDb = stx_download()
#' names(stxDb) # files downloaded from zenodo.org
#' View(stxDb)
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
#' 
stx_query_import = function(silent = TRUE, 
                            data_type = c('test_fin.fst','phch.fst','taxa.fst','refs.fst'),
                            ...) {
  if (!silent) message('Retrieving Standartox listed Chemicals ...')
  if (silent) {
    result = suppressMessages( stx_download(data_type = data_type, ...) )
  } else {
    result = stx_download(data_type = data_type, ...)
  }
  # Convert data frame to data table
  result = lapply( result, function(dt) data.table::setDT(dt) )
  return(result)
}

filter_dt = function(dt, var_ls){
  stopifnot(is.data.frame(dt))
  stopifnot(is.list(var_ls))
  data.table::setDT(dt) # Ensure dt is a data.table
  
  var_check = !unlist( lapply(var_ls, is.null) )
  var_check = names( which(var_check) ) # select only those variables that are not NULL
  
  # Loop through the variables and filter the taxa.fst data frame 
  if (length(var_check) > 0) {
    dt.out = list()
    
    for(var in var_check){
      message("Filtering for variable: ", var)
      if (var %in% colnames(dt)) { # Check if the variable exists as a column in data table
        if( !is.null(var_ls[[var]]) ){
          # Filter the data table based on the specified column and the values in the variable
          filter_val = var_ls[[var]]
          dt.out[[var]] <- dt[ dt[[var]] %in% filter_val ]
          if(nrow(dt.out[[var]]) == 0){ warning( paste("No query matches found for:\t", var) ) } 
        }
      } else {
        warning(paste("Variable", var, "not found in data table. Skipping filter."))
      }
    }
    
    # Combine results
    dt.out = distinct( data.table::rbindlist(dt.out) )
    # Check 
    if( nrow(dt.out) == 0 ) {
      warning("No query results for the provided ",paste(var_check, collapse = " & "),
              ". Please check the input values.")
      return(NULL) }
    return(dt.out)
  } else { dt }
}

stx_query = function(
  ## COMPOUND FILTERING ##
  cas_number = NULL,
  ## BASIC TOX DATA FILTERING ##
  endpoint_group = c('XX50', 'NOEX', 'LOEX'),
  exposure = NULL,           # character vector
  effect = NULL,             # character vector
  duration = c(0, Inf),      # numeric vector 
  duration_unit = "h",       # character vector; set to NULL if you want to keep all results!
  concentration_unit = NULL, # character vector
  concentration_type = NULL, # character vector
  ## TAXA FILTERING ##
  tax_columns = c('tax_group', 'tax_taxon', 'tax_genus', 'tax_family'), # Taxonomy columns to append to the query results. DEFAULT: c('tax_group', 'tax_taxon', 'tax_genus', 'tax_family')
  tax_genus  = NULL, # character vector
  tax_family = NULL, # character vector
  tax_order  = NULL, # character vector
  tax_class  = NULL, # character vector
  ecotox_grp = NULL, # character vector
  ## REFERENCE SECTION ##
  include_reference = FALSE, # Default FALSE
  ...){ 
  # Import stxDb 
  message("Reading in Standartox Data ...")
  stx_table = c('test_fin.fst','phch.fst','taxa.fst') # 
  if(include_reference) { stx_table = unique(c(stx_table,'refs.fst')) }
  stxDb = stx_query_import(data_type = stx_table, ...)
  # stxDb  = stx_query_import(data_type = stx_table)
  tox.dt = stxDb$test_fin.fst # final output object. LARGE right after import!
  stxDb  = stxDb[stx_table[-1]] # dump the largest object!
  
  # First quick filter steps:
  # Remove rows where the specified columns contain "NR" <- NA values
  message("Removing 'NA' values ...")
  tox.dt <- tox.dt[!grepl("NR", endpoint) & !grepl("NR", duration_unit) ]
  # Quick fix for endpoint values
  tox.dt[, endpoint := sub("[/*]+$","", endpoint)]
  if(!is.null(endpoint_group)){
    tmp_var = endpoint_group # quick fix
    tox.dt = tox.dt[endpoint_group %in% tmp_var]
  }
  if(!is.null(duration_unit)){
    tmp_var = duration_unit # quick fix
    tox.dt = tox.dt[duration_unit %in% tmp_var]
  }
  if( nrow(tox.dt) == 0 ) {
    warning("No query matches found for the provided endpoint_group or duration_unit. Please check the input values.")
  }
  
  # Step 1: Filter for cas_number then merge with toxdata
  message("Appending chemical information ...")
  ## Filter chem data for cas_number
  if(!is.null(cas_number)){
    stxDb$phch.fst <- stxDb$phch.fst[cas %in% cas_number]
    if( nrow(stxDb$phch.fst) == 0 ) {
      warning("No query matches found for the provided CAS numbers. Please check the input values.")
    }
  }
  ## Merge with tox data by cl_id
  merge(stxDb$phch.fst, tox.dt, all.x = TRUE, by = "cl_id") -> tox.dt
  tox.dt[, c("chem_class","casnr", "cl_id") := NULL] # don't need the cl_id column anymore.
  
  
  # Step 2: Filter for taxonomic groups then merge with toxdata
  message("Appending taxonomic information ...")
  ## Filtering ecotox_grp ##
  if(!is.null(ecotox_grp)){
    regstr = paste(ecotox_grp, collapse = "|")
    stxDb$taxa.fst = stxDb$taxa.fst[ grepl(regstr, stxDb$taxa.fst$tax_group) ]
    if( nrow( stxDb$taxa.fst ) == 0 ) {
      warning("No query matches found for the provided tax_group. Please check the input values.")
      return(NULL)
    }
  }
  ## Filtering tax_columns ##
  # Specify taxonomy columns for which tax filtering can be applied
  var_ls = list(
    tax_class  = tax_class,
    tax_order  = tax_order, 
    tax_family = tax_family,
    tax_genus  = tax_genus
  )
  tax.out = filter_dt( stxDb$taxa.fst, var_ls)
  if( is.null(tax.out) ) { return(NULL) } # Check 
  
  # Select pre-defined columns for output
  tax_key = "tl_id"
  tax.out = tax.out[, c(tax_key, tax_columns), with = FALSE]
  # Merge taxonomy data with tox data by tl_id
  merge(tax.out, tox.dt, all.x = TRUE, by = tax_key) -> tox.dt
  tox.dt[, (tax_key) := NULL] # drop columns here 
  
  
  # Step 3: Final Tox data filtering 
  # rmv any rows with NA in result_id
  tox.dt = tox.dt[!is.na(result_id)]
  # Filter for the selected columns 
  var_ls = list(
    concentration_unit  = concentration_unit,
    concentration_type  = concentration_type, 
    effect = effect,
    exposure  = exposure
  )
  tox.dt = filter_dt( tox.dt, var_ls )
  if( is.null(tox.dt) ) { return(NULL) } # Check 
  
  # Filter for duration
  lower_bound = min(duration)
  upper_bound = max(duration)
  tox.dt = tox.dt[duration >= lower_bound & duration <= upper_bound]
  # Check
  if( nrow( tox.dt ) == 0 ) {
    warning("No query matches found. Please check the input filter values.")
    return(NULL)
  }
  
  # Step 4: Append references if wanted 
  if(include_reference){
    message("Appending reference information ...")
    tox.dt = merge(tox.dt, stxDb$refs.fst, all.x = TRUE, by = "ref_number")
    tox.dt[, c("ref_number") := NULL]
  } 
  
  # Step 5: Final Cleanup 
  # Replace all occurrences of "NR" with NA
  tox.dt = tox.dt[, lapply(.SD, function(x) {
    if (is.character(x)) {
      x[x == "NR"] <- NA  # Replace "NR" with NA for character columns
    }
    return(x)  # Return the modified column
  })]
  # Filter out the "result_id" column
  message("Done!\n")
  return( tox.dt[, c("result_id") := NULL] )
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

#' Retrieve taxa groups
#' 
#' Retrieve a character vector of all tax_groups defined in Standartox.
#' 
#' @return Returns a character vector of all tax_groups defined in Standartox.
#' @author Hannes Reinwald \email{hannes.reinwald@@bayer.com}
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' stx_tax_groups()
#' }
#' 
#' @export
stx_tax_groups = function(...){
  taxa = na.omit( unique(stx_taxa(...)$tax_group) )
  taxa = unique( unlist( strsplit(taxa, "\\s*\\|\\|\\s*") ) )
  return(sort(taxa))
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


#' Retrieve meta data
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald \email{hannes.reinwald@@bayer.com}
#' @noRd
#' 
stx_meta = function(silent = TRUE, ...) {
  if (!silent) message('Retrieving Standartox listed Taxa ...')
  if (silent) {
    result = suppressMessages( stx_download(data_type = 'meta.fst', ...)[[1]] )
  } else {
    result = stx_download(data_type = 'meta.fst', ...)[[1]]
  }
  return(result)
}


# No longer needed I guess ... 
#' Check availability of connection
#' 
# #' @author Andreas Scharmüller
# #' @noRd
#' 
#stx_availability = function(res,
#                            http_codes = c(200, 400)) {
#  if (inherits(res, 'try-error') ||
#      ! httr::status_code(res) %in% http_codes) {
#    msg = paste0(
#    'The standartox web service seems currently unavailable.
#    Please try again after some time. Should it still not work then, please file an issue here:
#https://github.com/andschar/standartox/issues
#Error code: ', res)
#    stop(msg)
#  }
#}
