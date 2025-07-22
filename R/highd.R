#' Create a tibble with averaged high-dimensional data
#'
#' This function calculates the average values of high-dimensional data within each hexagonal bin.
#'
#' @param highd_data A tibble that contains the high-dimensional data.
#' @param scaled_nldr_hexid A tibble that contains the scaled embedding with hexagonal bin IDs.
#'
#' @return A tibble with the average values of the high-dimensional data within each hexagonal bin.
#'
#' @importFrom dplyr group_by summarise across select
#' @importFrom rsample starts_with
#' @importFrom tidyselect everything
#'
#' @examples
#' umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
#' avg_highd_data(highd_data = scurve, scaled_nldr_hexid = umap_with_hb_id)
#'
#' @export
avg_highd_data <- function(highd_data, scaled_nldr_hexid) {

  df_all <- dplyr::inner_join(highd_data, scaled_nldr_hexid, by = "ID")

  df_b <- df_all |>
    select(starts_with("x"), h) |>
    group_by(h) |>
    summarise(across(everything(), mean))

  return(df_b)
}

#' Create a tibble with averaged high-dimensional data and high-dimensional data
#'
#' This function combine the average values of high-dimensional data within each
#' hexagonal bin and high-dimensional data.
#'
#' @param highd_data A tibble that contains the high-dimensional data.
#' @param model_highd A tibble that contains the high-dimensional coordinates of bin centroids.
#' @param model_2d A tibble that contains hexagonal bin centroids in 2-D.
#'
#' @return A tibble with the average values of the high-dimensional data within
#' each hexagonal bin and high-dimensional data.
#'
#' @importFrom dplyr select mutate
#' @importFrom rsample starts_with
#'
#' @examples
#' comb_data_model(highd_data = scurve,
#' model_highd = scurve_model_obj$model_highd,
#' model_2d = scurve_model_obj$model_2d)
#'
#' @export
comb_data_model <- function(highd_data, model_highd, model_2d) {

  ### Define type column
  df <- highd_data |>
    select(starts_with("x")) |>
    mutate(type = "data") ## original dataset

  df_b <- model_highd |>
    filter(h %in% model_2d$h) |>
    mutate(type = "model") ## Data with summarized mean

  ## Reorder the rows of df_b according to the h order in model_2d
  df_b <- df_b[match(model_2d$h, df_b$h),] |>
    tidyr::drop_na() |>
    select(-h)

  df_exe <- bind_rows(df_b, df)

  df_exe

}


#' Visualise the model overlaid on high-dimensional data
#'
#' This function generates a langevitour which visualise the model
#' overlaid on high-dimensional data.
#'
#' @param point_data A tibble that contains the high-dimensional data and model in high-dimensions.
#' @param edge_data A tibble that contains the wireframe data (from and to).
#' @param point_colours A character vector that contains the colours of points in
#' the high-dimensional data and model.
#' @param point_sizes A numeric vector that contains the sizes of points in
#' the high-dimensional data and model.
#'
#' @return A langevitour object with the model and the high-dimensional data.
#'
#'
#' @examples
#' df_exe <- comb_data_model(highd_data = scurve, model_highd = scurve_model_obj$model_highd,
#' model_2d = scurve_model_obj$model_2d)
#' edge_data <- scurve_model_obj$trimesh_data
#' if (interactive()) {
#'   show_langevitour(point_data = df_exe, edge_data = edge_data)
#' }
#'
#' @export
show_langevitour <- function(point_data, edge_data,
                             point_colours = c("#66B2CC", "#FF7755"),
                             point_sizes = c(2, 1)) {

  df <- point_data |>
    dplyr::filter(type == "data") ## original dataset

  df_b <- point_data |>
    dplyr::filter(type == "model") ## High-d model

  langevitour::langevitour(point_data[1:(length(point_data)-1)],
                           lineFrom = edge_data$from,
                           lineTo = edge_data$to,
                           group = point_data$type,
                           pointSize = append(rep(point_sizes[1], NROW(df_b)),
                                              rep(point_sizes[2], NROW(df))),
                           levelColors = point_colours)


}
