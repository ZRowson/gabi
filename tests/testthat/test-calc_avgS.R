test_that("average speed calculation works", {

  # calculate values for average speed

  ## randomly select a fish from DNT60pmr0
  utils::data("DNT60pmr0")
  n <- nrow(DNT60pmr0); i <- sample(1:n, 1)
  fish <- DNT60pmr0[i,]

  ## calculate avgS values with calc_avgS
  calc_avgS_val <- gabi::calc_avgS(fish)

  ## calculate avgS values with another method

  ## get time period sets
  t <- grep("vt", names(DNT60pmr0), value = TRUE)
  g <- factor(c(rep(NA,10), rep(c("avgS_L","avgS_D"), each = 20)), levels = c("avgS_L", "avgS_D"))
  sets <- split(t, g); sets <- sets[c("avgS_L", "avgS_D")]
  sets[["avgS_T"]] <- t[11:50]

  ## calculate AUC for randomly drawn fish
  raw_avgS_val <- lapply(sets, function(set) mean(as.numeric(fish[set])))

  ## compare
  expect_equal(calc_avgS_val, raw_avgS_val)
})
