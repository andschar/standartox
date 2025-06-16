#' Download Standartox Data Tables from Zenodo.org
#' 
#' Downloads the Standartox data tables from Zenodo.org and reads them into R. Specific data_types can be specified.
#' 
#' @return Returns a list of data.tables containing the downloaded data. 
#'
#' @param data_type character; Specify the type of data to download. Select from c("meta", "phch", "refs", "test_fin", "taxa", "catalog"). NULL (default) will download and imports all,
#' @param dir_out character; Directory to which the downloaded files should be saved. Default is a temporary directory.
#' @param silent logical; If TRUE, suppresses messages. Default is TRUE.
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
stx_download = function(data_type = NULL, dir_out = file.path(tempdir(),"standartox"), silent = TRUE) {
  
  # please keep this. Makes it easier to quickly pull everything without the need of having to specify specific 
  stx_files = c("meta", "phch", "refs", "test_fin", "taxa", "catalog")
  if(is.null(data_type)){ data_type = stx_files}
  # Check
  data_type = match.arg( data_type, stx_files, several.ok = TRUE )
  
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
    if(!silent) message('\nChecking standartox file: ', n)
    
    # Define destination file path
    destfile = file.path(dir_out, n)
    
    # Check if the file already exists
    if ( !file.exists(destfile) ) {
      if(!silent) message('Downloading standartox ',n,' ...')
      curl::curl_download(url = URL,
                          destfile = destfile,
                          quiet = TRUE)
      if(!silent) message('Done! Downloaded to:\n', destfile)
    }
    else { if(!silent) message('File ', n, ' already exists, skipping download.') }
    
    if(!silent) message('Reading in file:\t', n)
    # Read in the downloaded files based on their extension 
    # for .fst: fst::read_fst(); for .rds: readRDS()
    sfx = sub("^.+[.]","", basename(destfile))
    if( sfx == "fst" ) { 
      stxDb_ls[[n]] = try ( fst::read_fst(destfile) )
      if(!silent) message("Done!\n")
    } else if (sfx == "rds") { 
      stxDb_ls[[n]] = readRDS(destfile)
      if(!silent) message("Done!\n")
    } else { warning("Unknown file format: ", sfx, "Expecting .fst or .rds files.\n") }
  }
  # Return the list of data frames
  return(stxDb_ls)
}


#' Filter data.table based on a list of variables
#' 
#' This function filters a data.table based on specified values in one or more columns. 
#' It checks for the existence of the specified columns and applies the filters accordingly.
#' 
#' @param dt data.table; The data.table to filter.
#' @param var_ls list; A named list where each element is a vector of values to filter the corresponding column in the data.table. The names of the list should match the column names in the data.table.
#' @param silent logical; If TRUE, suppresses messages. Default is TRUE.
#' 
#' @return Returns a filtered data.table containing only the rows that match the specified values in the columns. If no matches are found, a warning is issued and NULL is returned.
#' 
#' @author Hannes Reinwald
#' 
#' 
#' @examples
#' \donttest{
#' # Import the standartox taxonomy data table as example
#' taxa.dt = stx_taxa()
#' colnames(taxa.dt) # inspect column names to filter for
#' 
#' # Specify taxonomy columns for which tax filtering can be applied.
#' # Make sure that list names match the column names in your data table!
#' var_ls = list(
#' family = 'Cyprinidae',
#' genus  = c('Daphnia','Ceriodaphnia')
#' )
#' 
#' # Filter your taxonomy table for genus and family specified.
#' tax.out = filter_dt( taxa.dt, var_ls, silent = FALSE)
#' View(tax.out) # inspect the output
#' }
#' 
#' @noRd
filter_dt = function(dt, var_ls, silent = TRUE){
  stopifnot(is.data.frame(dt))
  stopifnot(is.list(var_ls))
  data.table::setDT(dt) # Ensure dt is a data.table
  
  var_check = !unlist( lapply(var_ls, is.null) )
  var_check = names( which(var_check) ) # select only those variables that are not NULL
  
  # Loop through the variables and filter the taxa.fst data frame 
  if (length(var_check) > 0) {
    dt.out = list()
    
    for(var in var_check){
      if(!silent) message("Filtering for variable: ", var)
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
    dt.out = unique( data.table::rbindlist(dt.out) )
    # Check 
    if( nrow(dt.out) == 0 ) {
      warning("No query results for the provided ",paste(var_check, collapse = " & "),
              ". Please check the input values.")
      return(NULL) }
    return(dt.out)
  } else { dt }
}


#' Query Standartox toxicity values
#'
#' Retrieve toxicity values from the Standartox data base on Zenodo.org \url{https://doi.org/10.5281/zenodo.3785030}.
#' 
#' @return Returns a data.table containing Standartox data base query results. 
#'
#' @param cas character, integer; Limit data base query to specific CAS numbers, multiple entries possible (e.g. c('1071-83-6', '1071836'), NULL (default).
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
#' 
#' @export
#' 
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
  rm_na = TRUE, # Default TRUE; if FALSE, keep NR values in the result
  verbose = FALSE, # Default TRUE; if FALSE, print messages
  ...){
  message("Querying Standartox data base ...")
  
  # Import stxDb 
  if(verbose) message("Reading in Standartox Data ...")
  stx_table = c('test_fin','phch','taxa') # 
  if(include_reference) { stx_table = unique(c(stx_table,'refs')) }
  stxDb =  stx_download(data_type = stx_table) #, ...)
  names(stxDb) = sub("[.]fst$","",names(stxDb)) # FIX
  
  # Convert data frame to data table
  stxDb = lapply( stxDb, function(dt) data.table::setDT(dt) )
  tox.dt = stxDb$test_fin # final output object. LARGE right after import!
  stxDb  = stxDb[stx_table[-1]] # dump the largest object! <- hope to save some memory with that.

  # First quick filter steps:
  # Remove rows where the specified columns contain "NR" <- NA values
  if(rm_na){
    if(verbose) message("Removing 'NA' values ...")
    tox.dt = tox.dt[!grepl("NR", endpoint) & !grepl("NR", duration_unit) ] 
  }
  
  # Quick fix for endpoint values - some contain weird string endings.
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
  if(verbose) message("Appending chemical information ...")
  ## Filter chem data for cas_number
  if(!is.null(cas_number)){
    stxDb$phch <- stxDb$phch[cas %in% cas_number]
    if( nrow(stxDb$phch) == 0 ) {
      warning("No query matches found for the provided CAS numbers. Please check the input values.")
    }
    merge(stxDb$phch, tox.dt, all.x = TRUE, by = "cl_id") -> tox.dt
  } else {
    merge(stxDb$phch, tox.dt, all.y = TRUE, by = "cl_id") -> tox.dt
  }
  suppressWarnings( tox.dt[, c("chem_class","casnr", "cl_id") := NULL] ) # don't need the cl_id column anymore.
  
  
  # Step 2: Filter for taxonomic groups then merge with toxdata
  if(verbose) message("Appending taxonomic information ...")
  
  # Append 'tax_' prefix to the taxonomic columns (except for tax_key)
  tax_key = "tl_id"
  tax_col = paste0("tax_", colnames(stxDb$taxa))
  tax_col[ grep(paste0("^tax_",tax_key,"$"),tax_col) ] <- tax_key # replace tax_key with tl_id
  colnames(stxDb$taxa) <- tax_col # set new column names
  
  # Select pre-defined columns for output
  tax.out = stxDb$taxa[, c(tax_key, tax_columns), with = FALSE]
  # Merge taxonomy data with tox data by tl_id
  tox.dt = merge(tox.dt, tax.out, all.x = TRUE, by = tax_key)
  
  ## Filtering ecotox_grp ##
  if(!is.null(ecotox_grp)){
    regstr = paste(ecotox_grp, collapse = "|")
    tox.dt = tox.dt[ grepl(regstr, tox.dt$tax_group) ]
    if( nrow( tox.dt ) == 0 ) {
      warning("No query matches found for the provided tax_group. Please check the input values.")
      return(NULL)
    }
  }
  ## Filtering tax_columns ##
  var_ls = list( # Specify taxonomy columns for which tax filtering can be applied
    tax_class  = tax_class,
    tax_order  = tax_order, 
    tax_family = tax_family,
    tax_genus  = tax_genus
  )
  tox.dt = filter_dt( tox.dt, var_ls)
  if( is.null(tox.dt) ) { return(NULL) } # Check 
  
  # Step 3: Final Tox data filtering 
  tox.dt = tox.dt[!is.na(result_id)] # rmv any rows with NA in result_id <- this should not be the case but to be save!
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
  lower = min(duration)
  upper = max(duration)
  if (lower != 0 | upper != Inf) {
    #tox.dt = tox.dt[duration %between% c(lower, upper)] # <- this works only when data.table is loaded
    tox.dt = tox.dt[duration >= lower & duration <= upper] # <- this works always
  }
  
  # Check
  if( nrow( tox.dt ) == 0 ) {
    warning("No query matches found. Please check the input filter values.")
    return(NULL)
  }
  
  # Step 4: Append references if wanted 
  if(include_reference){
    if(verbose) message("Appending reference information ...")
    tox.dt = merge(tox.dt, stxDb$refs, all.x = TRUE, by = "ref_number")
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
  ls = stx_download(data_type = 'catalog', dir_out = dir_out)[[1]]

  return(ls)
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
  if(!silent) message('Retrieving Standartox data..')
  out = stx_download(data_type = 'test_fin', dir_out = dir_out)[[1]]

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
  out = stx_download(data_type = 'phch', dir_out = dir_out)[[1]]

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
  out = stx_download(data_type = 'taxa', dir_out = dir_out)[[1]]

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
  out = stx_download(data_type = 'meta', dir_out = dir_out)[[1]]

  return(out)
}
