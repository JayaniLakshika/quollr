test_that("generate_edge_info() works", {
  suppressWarnings(tr_obj <- tripack::tri.mesh(x = c(1, 2, 3), y = c(4, 5, 6)))
  expect_snapshot(generate_edge_info(tr_obj))

})


