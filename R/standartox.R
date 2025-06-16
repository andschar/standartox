#' Download Standartox Data Tables from Zenodo.org
#' 
#' Downloads the Standartox data tables from Zenodo.org and reads them into R. Specific data_types can be specified.
#' 
#' @return Returns a list of data.tables containing the downloaded data. 
#'
#' @param data_type character; Specify the type of data to download. Can be one of NULL (default, downloads and imports all), "meta.fst", "phch.fst", "refs.fst", "test_fin.fst", "taxa.fst", etc.
#' @param dir_out character; Directory to which the downloaded files should be saved. Default is a temporary directory.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#' 
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' stxDb = stx_download()
#' names(stxDb) # files downloaded from zenodo.org
#' }
#' @noRd
#' 
stx_download = function(data_type, dir_out = file.path(tempdir(), "standartox")) {
  # Check
  data_type = match.arg(
    data_type,
    c("meta", "phch", "refs", "test_fin", "taxa", "catalog"),
    several.ok = TRUE
  )
  
  # Output directory
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  
  # Find download URLS
  # HACK this has to be done, because doi.org is the only permanent link between versions
  qurl_permanent = 'https://doi.org/10.5281/zenodo.3785030'
  req = httr::GET(qurl_permanent)
  cont = httr::content(req, as = 'text')
  
  # Extract all zenodo download links from the content
  qurl = unique(stringr::str_extract_all(cont, 'https://zenodo.org/records/[0-9]+/files/.+')[[1]])
  qurl = grep("[.]rds|[.]fst", qurl, value = TRUE) # filter for .rsd and .fst files only! 
  qurl = sub('\">', '', qurl, fixed = TRUE)
  qurl = grep(qurl, pattern = paste0(data_type, collapse = '|'), value = TRUE) # filter for data_type
  
  # For each link in qurl check if destination file exists and if not download it.
  l = list()
  for(URL in qurl){
    fl = basename(URL)
    fl_name = sub('.rds|.fst', '', fl) # remove file extension for list name

    # Define destination file path
    destfile = file.path(dir_out, fl)
    
    # Check if the file already exists
    if ( !file.exists(destfile) ) {
      curl::curl_download(url = URL,
                          destfile = destfile,
                          quiet = TRUE)
    }
    
    # Read in the downloaded files based on their extension 
    sfx = sub("^.+[.]", "", fl)
    if( sfx == "fst" ) { 
      out = fst::read_fst(destfile, as.data.table = TRUE)
    } else if (sfx == "rds") { 
      out = readRDS(destfile)
    } else { 
      stop("Unknown file format: ", sfx, "Expecting .fst or .rds files.")
    }
  }
  
  return(out)
}



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
  stxDb  = stx_query_import(data_type = stx_table, ...)
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
  tox.dt = tox.dt[duration %between% c(min(duration),max(duration))]
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



#' Retrieve data catalog
#' 
#' Retrieve a data catalog for all variables (and their values) that can be retrieved with stx_query()
#' 
#' @param silent logical; If TRUE, suppresses messages. Default is FALSE.
#' @param dir_out character; Directory to which the catalog should be downloaded. Default is a temporary directory.
#' 
#' @return Returns a list of data.frames containing information on data base variables
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#' 
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' l = stx_catalog()
#' 
#' # to get verbose output from the function
#' l = stx_catalog(silent = FALSE)
#' 
#' # to specify a directory to which the catalog should be downloaded
#' l = stx_catalog(silent = FALSE, dir_out = "~/tmp")
#' # This will create a directory under ~/tmp and download the catalog.rds file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' @export
#' 
stx_catalog = function(silent = FALSE, dir_out = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox catalog..')
  l = stx_download(data_type = 'catalog', dir_out = dir_out)

  return(l)
}

#' Retrieve Standartox toxicity values
#' 
#' Retrieve a data.table containing the Standartox toxicity data
#' 
#' @param silent logical; If TRUE, suppresses messages. Default is FALSE.
#' @param dir_out character; Directory to which the catalog should be downloaded. Default is a temporary directory.
#' 
#' @return Returns a data.table.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#' 
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' 
#' dt = stx_data()
#' 
#' }
#' @export
#' 
stx_data = function(silent = FALSE, dir_out = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox data..')
  out = stx_download(data_type = 'test_fin', dir_out = dir_out)

  return(out)
}

#' Retrieve chemical data
#' 
#' Retrieve data on all chemicals in Standartox.
#' 
#' @return Returns a data.table containing informaiton on chemicals in Standartox.
#' 
#' @param silent logical; If TRUE, suppresses messages. Default is FALSE.
#' @param dir_out character; Directory to which the chemical information should be downloaded. Default is a temporary directory.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#' 
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' df = stx_chem()
#' 
#' # to get verbose output from the function
#' df = stx_chem(silent = FALSE)
#' 
#' # to specify a directory to which the chemical information should be downloaded
#' df = stx_chem(silent = FALSE, dir_out = "~/tmp")
#' # This will create a directory under ~/tmp and download the respective standartox file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' 
#' @export
#' 
stx_chem = function(silent = FALSE, dir_out = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox chemical information..')
  out = stx_download(data_type = 'phch', dir_out = dir_out)

  return(out)
}

#' Retrieve taxa data
#' 
#' Retrieve data on all taxa in Standartox.
#' 
#' @return Returns a data.table containing informaiton on taxa in Standartox.
#' 
#' @param silent logical; If TRUE, suppresses messages. Default is FALSE.
#' @param dir_out character; Directory to which the taxa information should be downloaded. Default is a temporary directory.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#' 
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' df = stx_taxa()
#' 
#' # to get verbose output from the function
#' df = stx_taxa(silent = FALSE)
#' 
#' # to specify a directory to which the taxa information should be downloaded
#' df = stx_taxa(silent = FALSE, dir_out = "~/tmp")
#' # This will create a directory under ~/tmp and download the respective standartox file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' 
#' @export
#' 
stx_taxa = function(silent = FALSE, dir_out = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox taxa information..')
  out = stx_download(data_type = 'taxa', dir_out = dir_out)

  return(out)
}

#' Function to aggregate filtered test results
#'  
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
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
#' @return Returns a data.table containing meta informaiton on Standartox.
#' 
#' @param silent logical; If TRUE, suppresses messages. Default is FALSE.
#' @param dir_out character; Directory to which the meta information should be downloaded. Default is a temporary directory.#' 
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#' 
#' @examples
#' \donttest{
#' # might fail if there is no internet connection or Zenodo.org not not available
#' # basic function call
#' df = stx_meta()
#' 
#' # to get verbose output from the function
#' df = stx_meta(silent = FALSE)
#' 
#' # to specify a directory to which the taxa information should be downloaded
#' df = stx_meta(silent = FALSE, dir_out = "~/tmp")
#' # This will create a directory under ~/tmp and download the respective standartox file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' 
#' @export
#' 
stx_meta = function(silent = FALSE, dir_out = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox meta information..')
  out = stx_download(data_type = 'meta', dir_out = dir_out)

  return(out)
}
