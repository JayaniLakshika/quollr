
test_that("gen_scaled_data() works", {
  testthat::expect_snapshot(gen_scaled_data(data = s_curve_noise_umap,
                                            x = "UMAP1", y = "UMAP2"))
})
