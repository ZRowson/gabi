test_that("no data was lost during pmr0 to mc0 transformation", {

  # compare identifier data in pmr0 and mc0 for each observation

  ## columns to compare
  cols <- c('srcf', 'cpid', 'apid', 'rowi', 'coli', 'wllt', 'wllq', 'conc')

  ## load pmr0
  utils::data(DNT60pmr0)
  pmr0 <- DNT60pmr0[cols]

  ## randomly select an endpoint
  endps <- c("AUC_L", "AUC_D", "AUC_T", "AUC_r", "avgS_L", "avgS_D",
             "avgS_T", "avgA_L", "avgA_D", "avgJ_L", "avgJ_D","strtlA",
             "strtlAavg", "strtlF", "frzA","frzF")
  m <- length(endps); j <- sample(1:m, 1)
  rval <- endps[j]

  ## load mc0
  mc0 <- gabi::as_mc02(DNT60pmr0, rval = rval)[cols]

  expect_identical(mc0, pmr0)
})
