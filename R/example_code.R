# filter_dt -----------------------------------
taxa.dt = stx_taxa()
colnames(taxa.dt) # inspect column names to filter for

# Specify taxonomy columns for which tax filtering can be applied.
# Make sure that list names match the column names in your data table!
var_ls = list(
  family = 'Cyprinidae',
  genus  = c('Daphnia','Ceriodaphnia')
)

tax.out = filter_dt( taxa.dt, var_ls, silent = FALSE)
View(tax.out) # inspect the output


# stx_query ---------------------------------
# Note that by default stx_query filters for EC50, NOEC and LOEC values and for duration_unit = "h" (hours).
# if you need something else specify in filter!

# Example 1) - basix stx query
tox.dt1 = stx_query(verbose = T) # will return results filtered for duration_unit = "h" (hours)

# to keep all duration values, set duration_unit = NULL, default is "h"
tox.dt2 = stx_query(duration_unit = NULL, verbose = T) # keep NA values )

# To get everything!
tox.dt3 = stx_query( endpoint_group = NULL,
                     duration_unit = NULL, 
                     verbose = T, rm_na = F) # remove NA values

# NOTE: stx_data() will deliver ALL available results without any pre-filtering!
dt = stx_data()

# Example 2)
tox.dt = stx_query(
  verbose = T, 
  endpoint_group = "XX50",
  duration = c(12, 96),
  concentration_unit = "g/l", 
  tax_genus  = c("Danio","Pimephales") 
  #tax_family = c("Cyprinidae","Salmonidae")
  #tax_group  = c("fish","algae") 
)

# Example 3)
tox.dt = stx_query(
  verbose = T,
  cas_number = c("482-89-3", "50-00-0","1071-83-6"), 
  #endpoint_group = "XX50",
  duration = c(12, 96),
  concentration_unit = "g/l", 
  tax_genus = c("Danio","Daphnia"),
  include_reference = TRUE
)
