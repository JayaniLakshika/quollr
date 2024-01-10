test_that("generate_edge_info() works", {
  suppressWarnings(tr_obj <- tripack::tri.mesh(x = c(1, 2, 3), y = c(4, 5, 6)))
  expect_snapshot(generate_edge_info(tr_obj))
})

test_that("plots work", {
  df <- tibble::tribble(
    ~from, ~to, ~distance,
    1, 2, 5,
    1, 3, 12.2,
    2, 3, 8.25
  )
  tr_object <- tripack::tri.mesh(df$from, df$to)
  p1 <- colour_long_edges(df, 5, tr_object, "distance")
  vdiffr::expect_doppelganger("color_long_edges basic", p1)
})


test_that("extract_hexbin_centroids() works", {
  nldr_df <- s_curve_noise_umap
  num_bins <- 7
  shape_val <- 1.833091
  result <- extract_hexbin_centroids(nldr_df, num_bins, shape_val)
  hexdf_data <- result$hexdf_data
  hb_data <- result$hb_data
  expect_snapshot(hexdf_data)
  expect_snapshot(hb_data)
})
