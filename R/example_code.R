# filter_dt() -----------------------------------
taxa.dt = stx_taxa()
colnames(taxa.dt) # inspect column names to filter for

# Specify taxonomy columns for which tax filtering can be applied.
# Make sure that list names match the column names in your data table!
var_ls = list(
  family = "Cyprinidae",
  genus  = c("Daphnia","Ceriodaphnia")
)

tax.out = filter_dt( taxa.dt, var_ls, silent = FALSE)
View(tax.out) # inspect the output


# stx_query() ---------------------------------

# Example 1) - Basic stx_query() call
# will return results filtered for default endpoint_group = c("XX50", "NOEX", "LOEX") and duration_unit = "h"
dt = stx_query(verbose = T)

# If you wish to filter for different endpoint groups, you can specify them in the query.
stx_catalog()$endpoint_group # to view available endpoint groups
dt1 = stx_query(endpoint_group = c("Bioconc","MATC","MCIG"))

# if you wish to keep all duration_unit and endpoint_groups, set them to NULL. 
dt2 = stx_query(duration_unit = NULL, endpoint_group = NULL, verbose = T) 

# including "NR" (not reported) values. This is everything you can retrieve via stx_data() as well. 
dt3 = stx_query(duration_unit = NULL, endpoint_group = NULL, verbose = T, rm_NR = F) 


# Example 2) - Filter for specific taxonomic groups
# Filter for fish and algae for an exposure duration of 12 to 120 hours and concentration unit in g/l for endpoint group XX50 (50% effect/lethality values)
stx_catalog()$group # to get an idea about available taxonomic groups
stx_catalog()$concentration_unit
dt4 = stx_query(
  endpoint_group = "XX50",
  duration = c(12, 120),
  concentration_unit = "g/l", 
  tax_group  = c("fish","algae")
)


# Example 3) - Filter for specific genus and chemical compounds + append reference data
# make sure to extract only EC50/LC50 values from 12 - 96 hours and of comparable concentration units
dt5 = stx_query(
  cas_number = c("1071-83-6","63-25-2","138261-41-3"), # glyphosate, carbaryl and imidacloprid 
  endpoint_group = "XX50", # to get only 50% effect/lethality values
  duration = c(12, 96),
  concentration_unit = "g/l", 
  tax_genus = c("Danio","Daphnia","Ceriodaphnia","Chironomus"),
  include_reference = TRUE
)


# Example 4) - filter for specific taxonomic families and append reference data + more taxonomic lineage information
stx_catalog()$family # get an idea about which family to use by inspecting the taxa catalog
colnames(stx_taxa())[-1] # all possible taxonomic columns to extract from stx_taxa()

dt6 = stx_query(
  endpoint_group = "XX50",
  duration = c(12, 120),
  concentration_unit = "g/l", 
  tax_family = c("Cyprinidae","Salmonidae","Daphniidae"),
  tax_columns = c("group","taxon","genus","family","order","class","habitat"), # to get more taxonomic lineage information
  include_reference = TRUE
)


# Example 5) - Specific data selection
# get ALL LC50 values for 96 - 120 h of exposure for zebra fish (Danio rerio)
danio = stx_query(
  endpoint_group = "XX50",
  duration = c(96, 120),
  effect = "mortality",
  concentration_unit = "g/l",
  concentration_type = "active ingredient",
  tax_genus = "Danio",
  include_reference = TRUE
)

# get ALL LC50 values for 24 - 48 h of exposure for Family Daphniidae
dmag = stx_query(
  endpoint_group = "XX50",
  duration = c(24, 48),
  effect = "mortality",
  concentration_unit = "g/l",
  concentration_type = "active ingredient",
  #tax_genus = "Daphnia",
  tax_family = "Daphniidae",
  include_reference = TRUE
)
