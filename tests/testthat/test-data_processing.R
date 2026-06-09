set.seed(20240110)
test_that("gen_scaled_data() works", {
  testthat::expect_snapshot(gen_scaled_data(nldr_data = scurve_umap))
})
