test_that("error message works", {

  # test that correct
  # load testing data
  utils::data(DNT60pmr0)

  # check that correct error message is thrown

  expect_error(gabi::calc_endp(DNT60pmr0, rval = NULL), "rval improperly specified")

  expect_error(gabi::calc_endp(DNT60pmr0, rval = 0), "rval improperly specified")
})

test_that("correct rval is returned", {

  # calculate rval with gabi::calc_endp and by direct call to rval calculating function

  ## select random fish
  n <- nrow(DNT60pmr0); i <- sample(1:n, 1)
  fish <- DNT60pmr0[i,]

  ## select random endpoint
  endps <- c("AUC_L", "AUC_D", "AUC_T", "AUC_r", "avgS_L", "avgS_D",
                 "avgS_T", "avgA_L", "avgA_D", "avgJ_L", "avgJ_D","strtlA",
                 "strtlAavg", "strtlF", "frzA","frzF")
  m <- length(endps); j <- sample(1:m, 1)
  rval <- endps[j]

  ## calculate rval with calc_endp
  calc_endp_val <- gabi::calc_endp(DNT60pmr0, rval = rval)[i]

  ## calculate rval by function call
  no.A <- 10; no.L <- no.D <- 20
  if (rval %in% c("AUC_L", "AUC_D", "AUC_T", "AUC_r")) {
    raw_endp_val <- gabi::calc_AUC(fish, no.A, no.L, no.D)[[rval]]
  } else if (rval %in% c("avgS_L", "avgS_D", "avgS_T")) {
    raw_endp_val <- gabi::calc_avgS(fish, no.A, no.L, no.D)[[rval]]
  } else if (rval %in% c("avgA_L", "avgA_D", "avgJ_L", "avgJ_D")) {
    raw_endp_val <- gabi::calc_avgAJ(fish, no.A, no.L, no.D)[[rval]]
  } else if (rval %in% c("frzA","frzF","strtlA", "strtlAavg", "strtlF")) {
    raw_endp_val <- gabi::calc_trans(fish, no.A, no.L, no.D)[[rval]]
  }

  expect_equal(calc_endp_val, raw_endp_val)

})

test_that("no fish observations were deleted", {

  # compare nrow(DNT60pmr0) to length of endp

  ## select random endpoint
  endps <- c("AUC_L", "AUC_D", "AUC_T", "AUC_r", "avgS_L", "avgS_D",
             "avgS_T", "avgA_L", "avgA_D", "avgJ_L", "avgJ_D","strtlA",
             "strtlAavg", "strtlF", "frzA","frzF")
  m <- length(endps); j <- sample(1:m, 1)
  rval <- endps[j]

  ## get length of endp values
  calc_endp_n <- length(gabi::calc_endp(DNT60pmr0, rval = rval))

  ## compare to # of rows in DNT60pmr0
  raw_n <- nrow(DNT60pmr0)
  expect_equal(calc_endp_n, raw_n)
})
