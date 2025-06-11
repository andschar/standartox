# Useful Links: ----------------------------------------------------------------
#' REST API: 
#' https://developers.zenodo.org/#quickstart-upload
#' 
#' zen4R package vignette: 
#' https://cran.r-project.org/web/packages/zen4R/vignettes/zen4R.html
#' https://cran.r-project.org/web/packages/zen4R/zen4R.pdf 
#' 
#' 

# Testing zen4R ---------------------------------------------------------------
## Setup 
# Following the vignette here
# require("remotes")
# remotes::install_github("eblondel/zen4R")

# installed via CRAN
require("zen4R")
require(dplyr)

zenodo <- ZenodoManager$new() #The main entry point of zen4R is the ZenodoManager. 

# To use deposit functions of zen4R, you will need to specify the token.
# This token can be created here: 

# Import zenodo token INI file
INI = list.files(path = "ini", pattern = "^zenodo_token[.]ini$", 
                 full.names = TRUE, recursive = TRUE) %>% ini::read.ini(.)

zenodo <- ZenodoManager$new(
  token = INI$zenodo$access_token, #<your_token>, 
  logger = "INFO" # use "DEBUG" to see detailed API operation logs, use NULL if you don't want logs at all
)
# Great :) that works!
