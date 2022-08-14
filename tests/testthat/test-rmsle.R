test_that("rmsle is correct", {
  rmsle_calc <- rmsle_vec(c(60, 80, 90), c(67, 78, 91))
  rmsle_exp <- 0.064667924
  expect_equal(rmsle_calc, rmsle_exp)
})
