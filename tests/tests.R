# Standartox tests

# packages ----------------------------------------------------------------
require(standartox)
require(tinytest)

# Standartox catalog ------------------------------------------------------
catal = stx_catalog()
# class
expect_equal(class(catal), 'list')
expect_equal(class(catal$casnr), 'list')

# Standartox query --------------------------------------------------------
stx = stx_query(cas = '1071-83-6',
                concentration_unit = 'ug/l',
                concentration_type = 'active ingredient',
                duration = c(48, 96),
                endpoint = 'XX50',
                effect = 'mortality',
                exposure = 'aquatic')
# class
expect_equal(class(stx), 'list')
expect_equal(class(stx$filtered), c('data.table', 'data.frame'))

