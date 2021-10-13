testthat::test_that("AUC calculation works", {

  # compare calc_AUC output to other AUC calculation method

  ## randomly select a fish from DNT60pmr0
  utils::data("DNT60pmr0")
  n <- nrow(DNT60pmr0)
  i <- sample(1:n, 1)
  fish <- DNT60pmr0[i,]

  ## calculate AUC values with calc_AUC
  calc_AUC_val <- gabi::calc_AUC(fish)

  ## calculate AUC values with another method

  ## get time period sets
  t <- grep("vt", names(DNT60pmr0), value = TRUE)
  g <- factor(c(rep(NA,10), rep(c("AUC_L","AUC_D"), each = 20)), levels = c("AUC_L", "AUC_D"))
  sets <- split(t, g); sets <- sets[c("AUC_L", "AUC_D")]
  sets[["AUC_T"]] <- t[11:50]

  ## calculate AUC for randomly drawn fish
  raw_AUC_val <- lapply(sets, function(set) {
    fl <- set[c(1,length(set))] # first and last time periods of set
    sum(fish[fl])/2 + sum(fish[set[!(set%in%fl)]])
  })
  raw_AUC_val[["AUC_r"]] <- raw_AUC_val[["AUC_D"]] / (raw_AUC_val[["AUC_L"]] + 1)

  ## compare
  expect_equal(calc_AUC_val, raw_AUC_val)
})
