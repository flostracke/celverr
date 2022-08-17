test_that("rmsle_vec is correct", {
  rmsle_calc <- rmsle_vec(c(60, 80, 90), c(67, 78, 91))
  rmsle_exp <- 0.064667924
  expect_equal(rmsle_calc, rmsle_exp)
})

test_that("rmsle  is correct", {
  tibble_exp <- tibble::tibble(
    .metric = "rmsle",
    .estimator = "standard",
    .estimate = 0.064667924
  )

  tibble_calc <- tibble::tibble(
    actuals = c(60, 80, 90),
    predictions = c(67, 78, 91)
  ) %>%
    rmsle(actuals, predictions)

  expect_equal(tibble_exp, tibble_calc)
})


