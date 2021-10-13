test_that("calculation of average jerk and acceleration works", {

  # calculate values for average acceleration and speed

  ## randomly select a fish from DNT60pmr0
  utils::data("DNT60pmr0")
  n <- nrow(DNT60pmr0); i <- sample(1:n, 1)
  fish <- DNT60pmr0[i,]

  ## calculate avgAJ values with calc_avgAJ
  calc_avgAJ_val <- gabi::calc_avgAJ(fish)

  ## calculate avgAJ values with another method

  ## calculate acceleration and jerk data
  t <- grep("vt", names(fish), value = TRUE)
  data <- fish[t]; rownames(data) <- NULL
  adata <- lapply(2:50, function(col) data[,col] - data[,col-1])
  adata <- as.data.frame(adata, col.names = paste0("za",2:50))
  jdata <- lapply(2:49, function(col) adata[,col] - adata[,col-1])
  jdata <- as.data.frame(jdata, col.names = paste0("zj",3:50))

  ## get time period sets and calculate avgAJ
  raw_avgAJ_val <- list(avgA_L = mean(unlist(adata[,11:29])),
                        avgA_D = mean(unlist(adata[,31:49])),
                        avgJ_L = mean(unlist(jdata[,11:28])),
                        avgJ_D = mean(unlist(jdata[,31:48])))

  # compare
  expect_equal(calc_avgAJ_val, raw_avgAJ_val)
})
