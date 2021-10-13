test_that("transitory endpoints are calculated correctly", {

  # calculate values for transitory endpoints

  ## randomly select a fish from DNT60pmr0
  utils::data("DNT60pmr0")
  n <- nrow(DNT60pmr0); i <- sample(1:n, 1)
  fish <- DNT60pmr0[i,]

  ## calculate trans values with calc_trans
  calc_trans_val <- gabi::calc_trans(fish)

  ## calculate trans values with another method

  ## calculate acceleration and jerk data
  t <- grep("vt", names(fish), value = TRUE)
  data <- fish[t]; rownames(data) <- NULL
  strtlA <- unlist(data[,31] - data[,30])
  strtlAavg <- unlist(data[,31] - rowMeans(data[,11:30]))
  strtlF <- unlist(data[,31] / (data[,30]+1))
  frzA <- unlist(data[,11] - data[,10])
  frzF <- unlist(data[,11] / (data[,10]+1))

  ## get time period sets and calculate avgAJ
  raw_trans_val <- list(strtlA = strtlA,
                        strtlAavg = strtlAavg,
                        strtlF = strtlF,
                        frzA = frzA,
                        frzF = frzF)

  # compare
  expect_equal(calc_trans_val, raw_trans_val)
})
