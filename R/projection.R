#' Visualize a specific projection of langevitour
#'
#' This function visualize a specific projection of langevitour.
#'
#' @param points_df The tibble that contains the model and data.
#' @param projection The tibble of the projection.
#' @param edge_df The tibble that contains the edge information (from, to).
#' @return A ggplot object with the specific projection of langevitour.
#'
#' @importFrom ggplot2 ggplot geom_segment geom_point scale_color_manual aes labs
#'
#' @examples
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' num_bins_x <- 4
#' hb_obj <- hex_binning(data = s_curve_noise_umap_scaled, bin1 = num_bins_x,
#' r2 = r2)
#' all_centroids_df <- hb_obj$centroids
#' counts_df <- hb_obj$std_cts
#' df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
#' counts_df = counts_df) |>
#' dplyr::filter(drop_empty == FALSE)
#' tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")
#' tr_from_to_df <- gen_edges(tri_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_coord_df = tr_from_to_df, start_x = "x_from",
#' start_y = "y_from", end_x = "x_to", end_y = "y_to",
#' select_vars = c("from", "to", "distance"))
#' umap_data_with_hb_id <- hb_obj$data_hb_id
#' df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID),
#' umap_data_with_hb_id)
#' df_bin <- avg_highd_data(data = df_all, col_start = "x")
#' ### Define type column
#' df <- df_all |>
#'   dplyr::select(tidyselect::starts_with("x")) |>
#'   dplyr::mutate(type = "data") ## original dataset
#'
#' df_b <- df_bin |>
#'   dplyr::filter(hb_id %in% df_bin_centroids$hexID) |>
#'   dplyr::mutate(type = "model") ## Data with summarized mean
#'
#' ## Reorder the rows of df_b according to the hexID order in df_b_with_center_data
#' df_b <- df_b[match(df_bin_centroids$hexID, df_b$hb_id),] |>
#'   dplyr::select(-hb_id)
#'
#' df_exe <- dplyr::bind_rows(df_b, df)
#' benchmark <- 0.663
#'
#' ## Set the maximum difference as the criteria
#' distance_df_small_edges <- distance_df |>
#'   dplyr::filter(distance < benchmark) |>
#'   dplyr::select(-distance)
#'
#' projection_df <- cbind(
#' c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
#' c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))
#'
#'
#' gen_proj_langevitour(points_df = df_exe, projection = projection_df,
#' edge_df = distance_df_small_edges)
#'
#' @export
gen_proj_langevitour <- function(points_df, projection, edge_df) {

  ## To extract only points
  points_only_df <- points_df |>
    dplyr::select(-type)

  projected <- as.matrix(points_only_df) %*% projection
  projected_df <- projected |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::rename(c("proj1" = "...1",
                    "proj2" = "...2")) |>
    dplyr::mutate(type = points_df$type) |>
    dplyr::mutate(ID = dplyr::row_number())

  proj_model <- projected_df |>
    dplyr::filter(type == "model")

  model_df <- dplyr::left_join(edge_df, proj_model, by = c("from" = "ID"))
  names(model_df)[3:NCOL(model_df)] <- paste0(names(proj_model)[-NCOL(proj_model)], "_from")

  model_df <- dplyr::left_join(model_df, proj_model, by = c("to" = "ID"))
  names(model_df)[(NCOL(edge_df) + NCOL(proj_model)):NCOL(model_df)] <- paste0(names(proj_model)[-NCOL(proj_model)], "_to")

  proj_plot <- ggplot(data = projected_df, aes(x = proj1, y = proj2, color = type)) +
    geom_point(size = 2, alpha = 0.6) +
    geom_segment(data = model_df, aes(x = proj1_from, y = proj2_from, xend = proj1_to, yend = proj2_to), color = "#000000", alpha = 0.5) +
    scale_color_manual(values = c("#6a3d9a", "#33a02c"))

  return(proj_plot)

}
