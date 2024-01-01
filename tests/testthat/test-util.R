test_that("calculate_effective_x_bins() works", {

  set.seed(123)
  data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
  testthat::expect_equal(calculate_effective_x_bins(data, x), 6)

  testthat::expect_snapshot(calculate_effective_x_bins(data, x, cell_area = 0), error = TRUE)
  testthat::expect_snapshot(calculate_effective_x_bins(data, x, cell_area = Inf), error = TRUE)
  testthat::expect_snapshot(calculate_effective_x_bins(data, x, cell_area = -3), error = TRUE)

  data <- data |> dplyr::bind_rows(data.frame(x = NA, y = 2.5))
  testthat::expect_snapshot(calculate_effective_x_bins(data, x), error = TRUE)

  data <- data |> dplyr::filter(dplyr::row_number() != NROW(data)) |> dplyr::bind_rows(data.frame(x = Inf, y = 2.5))
  testthat::expect_snapshot(calculate_effective_x_bins(data, x), error = TRUE)


})


test_that("calculate_effective_shape_value() works", {

  set.seed(123)
  data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
  testthat::expect_equal(calculate_effective_shape_value(data, x, y), 0.96955502)

  data <- data |> dplyr::bind_rows(data.frame(x = NA, y = 2.5))
  #expect_error(calculate_effective_shape_value(data, x, y))
  testthat::expect_snapshot(calculate_effective_shape_value(data, x, y), error = TRUE)

  data <- data |> dplyr::bind_rows(data.frame(x = 1.4, y = NA))
  #expect_error(calculate_effective_shape_value(data, x, y))
  testthat::expect_snapshot(calculate_effective_shape_value(data, x, y), error = TRUE)

  data <- data |> dplyr::filter(!(dplyr::row_number() %in% c(301, 302)))

  data <- data |> dplyr::bind_rows(data.frame(x = Inf, y = 2.5))
  testthat::expect_snapshot(calculate_effective_shape_value(data, x, y), error = TRUE)

  data <- data |> dplyr::bind_rows(data.frame(x = 1.4, y = Inf))
  testthat::expect_snapshot(calculate_effective_shape_value(data, x, y), error = TRUE)

  data <- data |> dplyr::filter(dplyr::row_number() == 1)
  testthat::expect_snapshot(calculate_effective_shape_value(data, x, y), error = TRUE)

})


