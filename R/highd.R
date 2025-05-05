#' Create a dataframe with averaged high-dimensional data
#'
#' This function calculates the average values of high-dimensional data within each hexagonal bin.
#'
#' @param data A tibble that contains the high-dimensional data and embedding
#' with hexagonal bin IDs.
#'
#' @return A tibble with the average values of the high-dimensional data within each hexagonal bin.
#'
#' @importFrom dplyr group_by summarise across select
#' @importFrom rsample starts_with
#' @importFrom tidyselect everything
#'
#' @examples
#' umap_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' df_all <- dplyr::bind_cols(s_curve_noise_training, umap_data_with_hb_id)
#' avg_highd_data(data = df_all)
#'
#' @export
avg_highd_data <- function(data) {

  df_b <- data |>
    select(starts_with("x"), hb_id) |>
    group_by(hb_id) |>
    summarise(across(everything(), mean))

  return(df_b)
}

#' Create a dataframe with averaged high-dimensional data and high-dimensional data
#'
#' This function combine the average values of high-dimensional data within each
#' hexagonal bin and high-dimensional data.
#'
#' @param highd_data A tibble that contains the high-dimensional data.
#' @param model_highd A tibble that contains the high-dimensional coordinates of bin centroids.
#' @param model_2d The dataset with hexagonal bin centroids.
#'
#' @return A tibble with the average values of the high-dimensional data within
#' each hexagonal bin and high-dimensional data.
#'
#' @importFrom dplyr select mutate
#' @importFrom rsample starts_with
#'
#' @examples
#' df_bin_centroids <- s_curve_obj$s_curve_umap_model_obj$df_bin_centroids
#' df_bin <- s_curve_obj$s_curve_umap_model_obj$df_bin
#' comb_data_mode(highd_data = s_curve_noise_training, model_highd = df_bin,
#' model_2d = df_bin_centroids)
#'
#' @export
comb_data_model <- function(highd_data, model_highd, model_2d) {

  ### Define type column
  df <- highd_data |>
    select(starts_with("x")) |>
    mutate(type = "data") ## original dataset

  df_b <- model_highd |>
    filter(hb_id %in% model_2d$hexID) |>
    mutate(type = "model") ## Data with summarized mean

  ## Reorder the rows of df_b according to the hexID order in model_2d
  df_b <- df_b[match(model_2d$hexID, df_b$hb_id),] |>
    select(-hb_id)

  df_exe <- bind_rows(df_b, df)

  df_exe

}


#' Visualize the model overlaid on high-dimensional data
#'
#' This function generates a LangeviTour visualization based on different
#' conditions and input parameters.
#'
#' @param point_df A tibble that contains the high-dimensional data and model in high-dimensions.
#' @param edge_df A tibble that contains the wireframe data (from and to).
#'
#' @return A langevitour object with the model and the high-dimensional data.
#'
#'
#' @examples
#' df_exe <- comb_data_model(highd_data = s_curve_noise_training, model_highd = df_bin,
#' model_2d = df_bin_centroids)
#' edge_data <- s_curve_obj$s_curve_umap_model_tr_from_to_df
#' show_langevitour(point_df = df_exe, edge_df = edge_data)
#'
#' @export
show_langevitour <- function(point_df, edge_df) {

  df <- point_df |>
    dplyr::filter(type == "data") ## original dataset

  df_b <- point_df |>
    dplyr::filter(type == "model") ## High-d model

  langevitour::langevitour(point_df[1:(length(point_df)-1)],
                           lineFrom = edge_df$from,
                           lineTo = edge_df$to,
                           group = point_df$type,
                           pointSize = append(rep(2, NROW(df_b)), rep(1, NROW(df))),
                           levelColors = c("#000000", "#33a02c"))


}
