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
