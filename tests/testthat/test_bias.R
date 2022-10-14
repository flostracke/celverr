test_that("bias_vec is correct", {
  bias_calc <- bias_vec(c(205), c(225))
  bias_exp <- -20
  expect_equal(bias_calc, bias_exp)
})

test_that("bias  is correct", {
  tibble_exp <- tibble::tibble(
    .metric = "bias",
    .estimator = "standard",
    .estimate = -20
  )

  tibble_calc <- tibble::tibble(
    actuals = c(205),
    predictions = c(225)
  ) %>%
    bias(actuals, predictions)

  expect_equal(tibble_exp, tibble_calc)
})


