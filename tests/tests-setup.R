# Test setup script

if (!require(pacman)) {
  install.packages('pacman')  
}

# CRAN packages
pkg_cran = c('tinytest')

p_load(char = pkg_cran)

# Github packages
pkg_gh = c('andschar/standartox')

p_load_current_gh(pkg_gh)
