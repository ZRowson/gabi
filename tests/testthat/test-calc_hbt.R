test_that("habituation endpoints are calculated correctly", {

  # compare endpoint calculation by performing calculation by hand

  ## load DNT60pmr0
  utils::data(DNT60pmr0)

  ## randomly select an observation from DNT60pmr0
  n <- nrow(DNT60pmr0); i <- sample(1:n, 1)
  obs <- DNT60pmr0[i,]

  ## call gabi::calc_hbt()
  calc_hbt_val <- gabi::calc_hbt(obs)

  ## caclulate hbt values "by hand"
  t <- grep("vt", names(obs), value = TRUE)
  no.A <- 10; no.L <- no.D <- 20
  sets <- list(hbt_L = t[(no.A+1):(no.A+no.L)],
               hbt_D = t[(no.A+no.L+1):(no.A+no.L+no.D)])
  raw_hbt_val <- lapply(sets, function(set) {
                                temp <- unlist(obs[set])
                                min <- min(temp); max <- max(temp)
                                max / (min + 1)})

  # compare
  expect_equal(calc_hbt_val, raw_hbt_val)
})
