test_that("wape_vec is correct", {
  wape_calc <- wape_vec(c(100, 100, 100, 100), c(90, 105, 100, 110))
  wape_exp <- 6.25
  expect_equal(wape_calc, wape_exp)
})

test_that("wape  is correct", {
  tibble_exp <- tibble::tibble(
    .metric = "wape",
    .estimator = "standard",
    .estimate = 6.25
  )

  tibble_calc <- tibble::tibble(
    actuals = c(100, 100, 100, 100),
    predictions = c(90, 105, 100, 110)
  ) %>%
    wape(actuals, predictions)

  expect_equal(tibble_exp, tibble_calc)
})


