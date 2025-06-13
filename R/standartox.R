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
      l[[fl_name]] = fst::read_fst(destfile, as.data.table = TRUE)
    } else if (sfx == "rds") { 
      l[[fl_name]] = readRDS(destfile)
    } else { 
      stop("Unknown file format: ", sfx, "Expecting .fst or .rds files.")
    }
  }
  
  return(l)
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

  return(l[[1]])
}

#' Retrieve Standartox toxicity values
#' 
#' Retrieve a data.table contianing the Standartox toxicity data
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
#' l = stx_data()
#' 
#' }
#' @export
#' 
stx_data = function(silent = FALSE, dir_out = file.path(tempdir(), "standartox")) {
  if (!silent) message('Retrieving Standartox data..')
  l = stx_download(data_type = 'test_fin', dir_out = dir_out)

  return(l)
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
  l = stx_download(data_type = 'phch', dir_out = dir_out)

  return(l[[1]])
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
  l = stx_download(data_type = 'taxa', dir_out = dir_out)

  return(l[[1]])
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
  l = stx_download(data_type = 'meta', dir_out = dir_out)

  return(l[[1]])
}
