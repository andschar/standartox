#' Download and Cache Standartox Data Tables
#' 
#' Downloads data tables from the Standartox Zenodo repository, reads them into R, and caches them locally to avoid redundant downloads. The function dynamically resolves the latest file versions from the permanent DOI.
#'
#' @details
#' This function performs two key operations:
#' 1.  It resolves the permanent Standartox DOI (\url{https://doi.org/10.5281/zenodo.3785030}) to find the most recent file URLs on Zenodo, making it robust to future data updates.
#' 2.  It checks if a file already exists in the specified `stx_dir` directory. If it does, the download is skipped, and the local version is used, saving time and bandwidth.
#'
#' @param data_type character; Optional. A vector specifying the data tables to download. Choose from \code{c("meta", "phch", "refs", "test_fin", "taxa", "catalog")}. If \code{NULL} (the default), all available data tables are downloaded.
#' @param stx_dir character; Path to the directory for storing/caching the downloaded files. Defaults to a "standartox" subdirectory within the session's temporary directory (i.e., \code{file.path(tempdir(), "standartox")}).
#' @param silent logical; If \code{TRUE} (the default), suppresses progress messages during download and file reading.
#'
#' @return A named list of \code{data.table} objects. The names of the list elements correspond to the downloaded filenames (e.g., \code{"test_fin.fst"}).
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#'
#' @examples
#' \donttest{
#' # This function may fail if there is no internet connection or Zenodo.org is not available
#' 
#' # Download all data tables (to a temporary directory)
#' stxDb_all <- stx_download()
#' # The names of the list show the files that were loaded
#' names(stxDb_all)
#' 
#' # Download only specific data tables (e.g., chemical and taxonomy info)
#' stxDb_subset <- stx_download(data_type = c("phch", "taxa"))
#' names(stxDb_subset)
#' 
#' #' # Specify a permanent directory for storing the downloaded data tables
#' # This will create the directory if it doesn't exist and cache the files there
#' my_stx_dir <- file.path("~","my_standartox_db")
#' stxDb_permanent <- stx_download(stx_dir = my_stx_dir, silent = FALSE)
#' # Check the directory to see the cached files
#' list.files(my_stx_dir)
#' }
#' 
#' @export
stx_download = function(data_type = NULL, stx_dir = file.path(tempdir(),"standartox"), silent = TRUE) {
  
  # Please keep this! Makes it easier to quickly pull everything without the need of having to specify specific 
  stx_files = c("meta", "phch", "refs", "test_fin", "taxa", "catalog")
  if(is.null(data_type)){ data_type = stx_files}
  # Check
  data_type = match.arg( data_type, stx_files, several.ok = TRUE )
  
  # Check if the output directory exists, if not create it
  if (!dir.exists(stx_dir)) { dir.create(stx_dir, recursive = TRUE) }
  
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
    destfile = file.path(stx_dir, n)
    
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
      stxDb_ls[[n]] = try ( fst::read_fst(destfile, as.data.table = TRUE) )
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
#' tax.out = filter_dt.OR( taxa.dt, var_ls, silent = FALSE)
#' View(tax.out) # inspect the output
#' }
#' 
#' @noRd
filter_dt.OR = function(dt, var_ls, silent = TRUE){
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



#' Filter data.table based on a list of variables with AND logic
#' 
#' This function filters a data.table by sequentially applying filters for each column.
#' It returns only the rows that satisfy *all* specified filtering criteria.
#' 
#' @param dt data.table; The data.table to filter.
#' @param var_ls list; A named list where each element is a vector of values to filter by. The names of the list must match column names in the data.table.
#' @param silent logical; If TRUE, suppresses messages. Default is TRUE.
#' 
#' @return Returns a filtered data.table containing only the rows that match all specified criteria. If no matches are found, a warning is issued and an empty data.table is returned.
#' 
#' @author Hannes Reinwald (revised for 'AND' logic)
#' 
#' @examples
#' \donttest{
#' # Import the standartox taxonomy data table as example
#' taxa.dt = stx_taxa()
#' colnames(taxa.dt) # inspect column names to filter for
#' 
#' # Specify filters. We want rows where family is 'Daphniidae' AND
#' # genus is 'Daphnia'.
#' var_ls <- list(
#'   tax_family = 'Daphniidae',
#'   tax_genus  = 'Daphnia'
#' )
#' 
#' # This should return only the 'Daphnia magna' row.
#' tax.out <- filter_dt.AND(taxa.dt, var_ls, silent = FALSE)
#' print(tax.out) 
#' }
#' 
#' @noRd
filter_dt.AND <- function(dt, var_ls, silent = TRUE) {
  # --- Initial Checks ---
  stopifnot(is.data.frame(dt))
  stopifnot(is.list(var_ls))
  dt_out <- data.table::setDT(data.table::copy(dt)) # Use a copy to avoid modifying the original dt by reference
  
  # --- Identify valid filters ---
  # Select only those variables that are not NULL in the list
  var_check <- names(var_ls)[!sapply(var_ls, is.null)]
  
  if (length(var_check) == 0) {
    if (!silent) message("No filters provided in var_ls. Returning original data.table.")
    return(dt_out)
  }
  
  # --- Iteratively apply filters (AND logic) ---
  for (var in var_check) {
    if (!var %in% colnames(dt_out)) {
      warning("Column '", var, "' not found in data.table. Skipping this filter.")
      next # Skip to the next iteration
    }
    
    filter_val <- var_ls[[var]]
    if (!silent) message("Applying filter on '", var, "'...")
    
    # This is the key change: filter the already-filtered data.table
    dt_out <- dt_out[dt_out[[var]] %in% filter_val]
    
    # Early exit if a filter results in zero rows
    if (nrow(dt_out) == 0) {
      if (!silent) {
        warning("Filter on '", var, "' resulted in 0 rows. No final matches possible.")
      }
      return(dt_out) # Return the empty data.table
    }
  }
  
  return(dt_out)
}



#' Query and Filter Standartox Toxicity Data
#'
#' Retrieves and filters toxicity data from the Standartox database (\url{https://doi.org/10.5281/zenodo.3785030}). This function acts as a powerful front-end for subsetting the database based on chemical, experimental, and taxonomic criteria.
#'
#' @details
#' The function operates in a sequential process:
#' 1.  It first downloads the necessary data tables (`test_fin`, `phch`, `taxa`, and optionally `refs`) using \code{\link{stx_download}}.
#' 2.  It performs initial, fast filtering based on `endpoint_group`, `endpoint_qualifier`, and `duration_unit`.
#' 3.  It then appends chemical and taxonomic information, filtering by `cas_number` and the `tax_*` parameters.
#' 4.  Finally, it applies the remaining experimental filters (`effect`, `duration`, `concentration_unit`, etc.).
#'
#' By default, filters are combined with a logical "AND". For example, specifying a `tax_genus` and an `effect` will return only records that match both criteria.
#'
#' `stx_catalog()` is a helper function that provides a list of all valid filter values for many of this function's parameters.
#'
#' @param cas_number character; Optional vector of CAS numbers to filter by (e.g., \code{c("1071-83-6", "63-25-2")}). If `NULL` (default), results for all chemicals are returned.
#' @param endpoint_group character; Optional vector of endpoint groups to filter results. See \code{stx_catalog()$endpoint_group} for all possible values. Defaults to \code{c("XX50", "NOEX", "LOEX")}.
#' @param endpoint_qualifier character; Optional vector of endpoint value qualifiers (e.g., \code{c(">", "<=")}). Common values are "=", ">", "<", "~", "<=", ">=". Defaults to \code{"="}. Set to `NULL` to include all qualifiers.
#' @param endpoint character; Optional vector of specific endpoints to filter by (e.g., "LC50", "NOEC"). If `NULL` (default), no endpoint filtering is applied.
#' @param exposure character; Optional vector of exposure types. See \code{stx_catalog()$exposure}. If `NULL` (default), no exposure type filtering is applied.
#' @param effect character; Optional vector of observed effects. See \code{stx_catalog()$effect}. If `NULL` (default), no effect filtering is applied.
#' @param measurement character; Optional vector of measurement types. See \code{stx_catalog()$measurement}. If `NULL` (default), no measurement filtering is applied.
#' @param duration numeric; A numeric vector of length two specifying the duration range (in hours) to filter by, e.g., \code{c(24, 96)}. Defaults to \code{c(0, Inf)}, including all durations.
#' @param duration_unit character; Filter by the unit of duration. See \code{stx_catalog()$duration_unit}. Defaults to \code{"h"} (hours). Set to `NULL` to keep all units.
#' @param concentration_unit character; Optional vector of concentration units. See \code{stx_catalog()$concentration_unit}. If `NULL` (default), no filtering is applied.
#' @param concentration_type character; Optional vector of concentration types. See \code{stx_catalog()$concentration_type}. If `NULL` (default), no filtering is applied.
#' @param organism_lifestage character; Optional vector of organism lifestages. See \code{stx_catalog()$organism_lifestage}. If `NULL` (default), no lifestage filtering is applied.
#' @param tax_columns character; A vector of taxonomic column names to append to the results. See \code{colnames(stx_taxa())} for all options. Defaults to \code{c("tax_group", "tax_taxon", "tax_genus", "tax_family")}.
#' @param tax_genus character; Optional vector of genera to filter by. See \code{stx_catalog()$genus}. If `NULL` (default), no genus filtering is applied.
#' @param tax_family character; Optional vector of families to filter by. See \code{stx_catalog()$family}. If `NULL` (default), no family filtering is applied.
#' @param tax_order character; Optional vector of orders to filter by. See \code{stx_catalog()$order}. If `NULL` (default), no order filtering is applied.
#' @param tax_class character; Optional vector of classes to filter by. See \code{stx_catalog()$class}. If `NULL` (default), no class filtering is applied.
#' @param tax_group character; Optional vector of ecotoxicological groups to filter by. See \code{stx_catalog()$tax_group} for valid inputs (e.g., "invertebrate", "fish"). If `NULL` (default), no group filtering is applied.
#' @param include_reference logical; If `TRUE`, reference information (author, year, title) is appended to the results. Defaults to \code{FALSE}.
#' @param rm_NR logical; If `TRUE` (default), rows with "NR" (not reported) values in the critical `endpoint` and `duration_unit` columns are removed early in the query process. Note that at the end of the query, all remaining "NR" values in any character column are converted to `NA`.
#' @param verbose logical; If `TRUE`, prints messages detailing the query progress. Defaults to \code{FALSE}.
#' @param ... Additional arguments to be passed on to \code{\link{stx_download}}, such as `stx_dir` to specify a cache directory.
#'
#' @return A \code{data.table} containing the filtered Standartox toxicity data. If the query results in zero matches, the function returns `NULL` and issues a warning.
#'
#' @author Hannes Reinwald
#'
#' @examples
#' \donttest{
#' # This function may fail if there is no internet connection or Zenodo.org is not available
#' 
#' # Basic query using default filters (XX50, NOEX, LOEX endpoints in hours)
#' # Using verbose=TRUE to see the process
#' results_default <- stx_query(verbose = TRUE)
#' 
#' # To see available filter options, use the catalog function
#' catalog <- stx_catalog()
#' print(catalog$endpoint_group)
#' 
#' # Query for a specific CAS number and multiple specific endpoints for key taxonomic groups
#' q1 <- stx_query(
#'   cas_number = "1071-83-6", # Glyphosate
#'   endpoint = c("LC50", "EC50", "LOEC", "NOEC"),
#'   duration = c(0, 120), # Up to 120 hours
#'   tax_group = c("invertebrate", "fish", "algae")
#' )
#' 
#' # Get all >LC50 values for Zebra fish (Danio rerio) embryos or larvae,
#' # including reference information
#' q2 <- stx_query(
#'   endpoint = "LC50",
#'   endpoint_qualifier = ">",
#'   duration = c(72, 120),
#'   tax_genus = "Danio",
#'   organism_lifestage = c("Embryo", "Larva"),
#'   include_reference = TRUE,
#'   verbose = TRUE
#' )
#' 
#' # Get all XX50 acute toxicity values for the family Daphniidae related to mobility
#' q3 <- stx_query(
#'   endpoint_group = "XX50",
#'   duration = c(24, 48),
#'   measurement = c("immobile", "mobility", "swimming"),
#'   tax_family = "Daphniidae",
#'   include_reference = TRUE
#' )
#' }
#'
#' @export
stx_query = function(
    ## COMPOUND FILTERING ##
  cas_number = NULL,
  ## BASIC TOX DATA FILTERING ##
  endpoint_group = c('XX50', 'NOEX', 'LOEX'),
  endpoint_qualifier = "=",  # *NEW - character vector; any of c("=",">","<","~","<=",">="). Set to NULL if you want to keep all results!
  endpoint = NULL,           # *NEW - character vector
  exposure = NULL,           # character vector
  effect = NULL,             # character vector
  measurement = NULL,        # *NEW - character vector
  duration = c(0, Inf),      # numeric vector 
  duration_unit = "h",       # character vector; set to NULL if you want to keep all results!
  concentration_unit = NULL, # character vector
  concentration_type = NULL, # character vector
  organism_lifestage = NULL, # *NEW - character vector
  ## TAXA FILTERING ##
  tax_columns = c('tax_group', 'tax_taxon', 'tax_genus', 'tax_family'), # Taxonomy columns to append to the query results. One of colnames(stx_taxa())
  tax_genus  = NULL, # character vector
  tax_family = NULL, # character vector
  tax_order  = NULL, # character vector
  tax_class  = NULL, # character vector
  tax_group  = NULL, # character vector
  ## REFERENCE SECTION ##
  include_reference = FALSE, # Default FALSE
  rm_NR = TRUE, # Default TRUE; if FALSE, keep NR values in the result
  verbose = FALSE, # Default TRUE; if FALSE, print messages
  ...){
  message("Querying Standartox data base ...")
  
  # Import stxDb 
  if(verbose) message("Reading in Standartox Data ...")
  stx_table = c('test_fin','phch','taxa') # 
  if(include_reference) { stx_table = unique(c(stx_table,'refs')) }
  stxDb =  stx_download(data_type = stx_table) #, ...)
  names(stxDb) = sub("[.]fst$","",names(stxDb)) # FIX
  
  # Split up list object
  total_entries = nrow(stxDb$test_fin)
  tox.dt = stxDb$test_fin # final output object. LARGE right after import!
  suppressWarnings( tox.dt[, casnr := NULL] ) # HOT FIX!
  stxDb  = stxDb[stx_table[-1]] # dump the largest object! <- hope to save some memory with that.
  
  # First quick filter steps:
  # Remove rows where the specified columns contain "NR" <- NA values
  if(rm_NR){
    if(verbose) message("Removing rows with 'NR' (not reported) for endpoint & duration_unit ...")
    tox.dt = tox.dt[endpoint != "NR" & duration_unit != "NR"] # <- this should not be the case but to be save!
  }
  
  if(!is.null(endpoint_group)){
    tmp_var = endpoint_group     # quick fix
    tox.dt = tox.dt[endpoint_group %in% tmp_var]
  }
  
  if(!is.null(endpoint_qualifier)){
    tmp_var = endpoint_qualifier # quick fix
    tox.dt = tox.dt[qualifier %in% tmp_var]
  }
  
  if(!is.null(duration_unit)){
    tmp_var = duration_unit      # quick fix
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
  
  tax_key = "tl_id"
  
  # Check if the provided tax_columns + tax_key are element of the taxa table. If any is not matching:
  # return a WARNING letting the user know which column did not match! <- was then removed!
  tmp_check = tax_columns %in% colnames(stxDb$taxa)
  if( !all(tmp_check) ){
    warning("The following non-matching column names were identified in 'tax_columns':\n",
            paste(tax_columns[!tmp_check], collapse = ", "),"\nFor these columns no filtering could be applied!")
    tax_columns = tax_columns[tmp_check] }
  
  # Select pre-defined columns for output
  tax.out = stxDb$taxa[, c(tax_key, tax_columns), with = FALSE]
  # Merge taxonomy data with tox data by tl_id
  tox.dt = merge(tox.dt, tax.out, all.x = TRUE, by = tax_key)
  
  ## Filtering ecotox_grp ##
  if(!is.null(tax_group)){
    regstr = paste(tax_group, collapse = "|")
    tox.dt = tox.dt[ grepl(regstr, tox.dt$tax_group) ]
    if( nrow( tox.dt ) == 0 ) {
      warning("No query matches found for the provided tax_group. Please check the input values.")
      return(NULL)
    }
  }
  ## Filtering tax_columns ##
  var_ls = list( 
    # Specify taxonomy columns for which tax filtering can be applied
    tax_class  = tax_class,
    tax_order  = tax_order, 
    tax_family = tax_family,
    tax_genus  = tax_genus
  )
  tox.dt = filter_dt.AND( tox.dt, var_ls)
  if( is.null(tox.dt) ) { return(NULL) } # Check
  suppressWarnings( tox.dt[, (tax_key) := NULL] ) # don't need the tl_id column anymore.
  
  # Step 3: Final Tox data filtering 
  tox.dt = tox.dt[!is.na(result_id)] # rmv any rows with NA in result_id <- this should not be the case but to be save!
  # Filter for the selected columns 
  var_ls = list(
    # filter attributes can be simply added below:
    concentration_unit  = concentration_unit,
    concentration_type  = concentration_type, 
    effect = effect,
    exposure  = exposure,
    endpoint = endpoint,                    # NEW <- add as variable to the function!
    measurement = measurement,              # NEW <- add as variable to the function!
    organism_lifestage = organism_lifestage # NEW <- add as variable to the function!
  )
  tox.dt = filter_dt.AND( tox.dt, var_ls ) # <- something fishy with this function ... 
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
  
  message("Query returned ",nrow(tox.dt)," results out of ",total_entries," total entries.\nDone!\n")
  return( tox.dt[, c("result_id") := NULL] ) # Filter out the "result_id" column
} 




#' Retrieve Standartox Data Catalog
#' 
#' Retrieves a catalog of all possible values for variables that can be used for filtering in \code{stx_query()}. This is useful for discovering valid inputs for parameters like \code{endpoint_group}, \code{effect}, or \code{tax_group}.
#' 
#' @param silent logical; If \code{TRUE}, suppresses messages during the download process. Default is \code{FALSE}.
#' @param stx_dir character; Directory where the catalog file is stored. If the file doesn't exist, it will be downloaded to this location. Defaults to a "standartox" subdirectory within the session's temporary directory (\code{tempdir()}).
#' 
#' @return Returns a list where each element is a character vector containing the unique values for a specific data field available for querying.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#' 
#' @examples
#' \donttest{
#' # This function might fail if there is no internet connection or Zenodo.org is not available
#' 
#' # Basic function call to retrieve the catalog
#' catalog_data <- stx_catalog()
#' 
#' # View the names of available fields in the catalog
#' names(catalog_data)
#' 
#' # See all possible values for 'endpoint_group'
#' print(catalog_data$endpoint_group)
#' 
#' # Get verbose output from the function
#' catalog_verbose <- stx_catalog(silent = FALSE)
#' 
#' # Specify a permanent directory to download and cache the catalog file.
#' # This speeds up future calls as the file won't need to be re-downloaded when starting a new session.
#' my_dir <- file.path("~","my_stx_data")
#' catalog_cached <- stx_catalog(stx_dir = my_dir, silent = FALSE)
#' }
#' 
#' @export
stx_catalog = function(silent = FALSE, stx_dir = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox catalog..')
  ls = stx_download(data_type = 'catalog', stx_dir = stx_dir)[[1]]
  return(ls)
}



#' Retrieve the Core Standartox Toxicity Data Table
#'
#' Provides direct access to the main `test_fin` data table from the Standartox database, which contains the raw toxicity results.
#'
#' @details
#' This function is a simple wrapper for \code{stx_download(data_type = 'test_fin')}. It is designed for users who want the primary data table of toxicity test results without the additional chemical or detailed taxonomic information that \code{\link{stx_query}} automatically appends.
#'
#' Like other download functions in this package, it caches the data file locally to avoid re-downloading on subsequent calls.
#'
#' @param silent logical; If `TRUE`, suppresses progress messages during download. Defaults to `FALSE`.
#' @param stx_dir character; Directory where the data file is stored or should be downloaded. Defaults to a "standartox" subdirectory within the session's temporary directory (\code{file.path(tempdir(), "standartox")}).
#'
#' @return A \code{data.table} containing the core toxicity test results, with columns for endpoints, duration, concentration, effect, etc.
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#'
#' @examples
#' \donttest{
#' # This function may fail if there is no internet connection or Zenodo.org is not available
#'
#' # Basic function call to retrieve the main data table
#' tox_data <- stx_data()
#'
#' # Inspect the first few rows and column names to understand the structure
#' head(tox_data)
#' colnames(tox_data)
#'
#' # Example of using a permanent directory to cache the data
#' # The message "Retrieving Standartox data.." will only appear on the first download.
#' my_dir <- file.path("~", "my_stx_data")
#' cached_data <- stx_data(stx_dir = my_dir, silent = FALSE)
#' }
#' @export
stx_data = function(silent = FALSE, stx_dir = file.path(tempdir(), "standartox")) {
  if(!silent) message('Retrieving Standartox data..')
  out = stx_download(data_type = 'test_fin', stx_dir = stx_dir)[[1]]
  return(out)
}



#' Retrieve the Standartox Chemical Properties Table
#'
#' Provides direct access to the `phch` data table, which contains chemical identifiers and physicochemical properties for all compounds in the Standartox database.
#'
#' @details
#' This function is a simple wrapper for \code{stx_download(data_type = 'phch')}. It is designed for users who want to explore the chemical inventory of the database, for instance, to find CAS numbers for use with \code{\link{stx_query}}.
#'
#' Like other download functions in this package, it caches the data file locally to avoid re-downloading on subsequent calls.
#'
#' @param silent logical; If `TRUE`, suppresses progress messages during download. Defaults to `FALSE`.
#' @param stx_dir character; Directory where the chemical data file is stored or should be downloaded. Defaults to a "standartox" subdirectory within the session's temporary directory (\code{file.path(tempdir(), "standartox")}).
#'
#' @return A \code{data.table} containing chemical information, including columns such as CAS number, chemical name, and other identifiers.
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#'
#' @examples
#' \donttest{
#' # This function may fail if there is no internet connection or Zenodo.org is not available
#'
#' # Basic function call to retrieve the chemical data table
#' chem_data <- stx_chem()
#'
#' # Inspect the first few rows and column names to see what's available
#' head(chem_data)
#' colnames(chem_data)
#'
#' # Example of using a permanent directory to cache the data file
#' # The "Retrieving..." message will only appear on the first download.
#' my_dir <- file.path("~", "my_stx_data")
#' cached_chem_data <- stx_chem(stx_dir = my_dir, silent = FALSE)
#' }
#' 
#' @export
stx_chem = function(silent = FALSE, stx_dir = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox chemical information..')
  out = stx_download(data_type = 'phch', stx_dir = stx_dir)[[1]]
  return(out)
}



#' Retrieve the Standartox Taxonomy Table
#'
#' Provides direct access to the `taxa` data table, which contains the complete taxonomic classification for every species in the Standartox database.
#'
#' @details
#' This function is a simple wrapper for \code{stx_download(data_type = 'taxa')}. It is designed for users who want to explore the full taxonomic scope of the database or find valid inputs for the `tax_genus`, `tax_family`, and `tax_group` parameters in \code{\link{stx_query}}.
#'
#' Like other download functions in this package, it caches the data file locally to avoid re-downloading on subsequent calls.
#'
#' @param silent logical; If `TRUE`, suppresses progress messages during download. Defaults to `FALSE`.
#' @param stx_dir character; Directory where the taxonomy data file is stored or should be downloaded. Defaults to a "standartox" subdirectory within the session's temporary directory (\code{file.path(tempdir(), "standartox")}).
#'
#' @return A \code{data.table} containing taxonomic classifications, with columns such as `tax_genus`, `tax_family`, `tax_order`, and `tax_group`.
#'
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' @author Hannes Reinwald
#'
#' @examples
#' \donttest{
#' # This function may fail if there is no internet connection or Zenodo.org is not available
#'
#' # Basic function call to retrieve the taxonomy data table
#' taxa_data <- stx_taxa()
#'
#' # Inspect the first few rows and see all available columns
#' head(taxa_data)
#' colnames(taxa_data)
#'
#' # Find all unique ecotoxicological groups in the database
#' if (nrow(taxa_data) > 0) {
#'   print(unique(taxa_data$tax_group))
#' }
#'
#' # Example of using a permanent directory to cache the data file
#' my_dir <- file.path("~", "my_stx_data")
#' cached_taxa_data <- stx_taxa(stx_dir = my_dir, silent = FALSE)
#' }
#' 
#' @export
stx_taxa = function(silent = FALSE, stx_dir = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox taxa information..')
  out = stx_download(data_type = 'taxa', stx_dir = stx_dir)[[1]]
  return(out)
}



#' Function to aggregate filtered test results
#'  
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
#' @noRd
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
#' @param stx_dir character; Directory to which the meta information should be downloaded. Default is a temporary directory.#' 
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
#' df = stx_meta(silent = FALSE, stx_dir = "~/tmp")
#' # This will create a directory under ~/tmp and download the respective standartox file to that directory.
#' # The files are then permanently stored in that directory and can be directly read when restarting your R session.
#' }
#' 
#' @export
stx_meta = function(silent = FALSE, stx_dir = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox meta information..')
  out = stx_download(data_type = 'meta', stx_dir = stx_dir)[[1]]
  return(out)
}