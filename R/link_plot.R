#' Visualize the model overlaid on high-dimensional data along with 2D wireframe model.
#'
#' This function generates a LangeviTour visualization based on different
#' conditions and input parameters with 2D wireframe.
#'
#' @param df_all A tibble that contains the high-dimensional data and embedding data.
#' @param df_b A tibble that contains the high-dimensional coordinates of bin centroids.
#' @param df_b_with_center_data The dataset with hexagonal bin centroids.
#' @param benchmark_value The benchmark value used to remove long edges (optional).
#' @param distance_df The tibble with distance.
#' @param distance_col The name of the distance column.
#' @param use_default_benchmark_val Logical, indicating whether to use default
#' benchmark value  to remove long edges (default is FALSE).
#' @param col_start The text that begin the column name of the high-dimensional data.
#' @param type_nldr The type of non-linear dimensionality reduction (NLDR) used.
#' @param r2 The ratio of the ranges of the original embedding components.
#'
#'
#' @return A browsable HTML element.
#'
#' @importFrom dplyr mutate bind_rows filter select
#' @importFrom langevitour langevitour
#' @importFrom ggplot2 ggplot theme_linedraw aes_string theme element_rect element_text element_blank
#' @importFrom plotly ggplotly config highlight style
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
#' df_all <- df_all |> dplyr::select(-ID, -hb_id)
#' show_link_plots(df_all = df_all, df_b = df_bin, df_b_with_center_data = df_bin_centroids,
#' benchmark_value = 1.16, distance = distance_df, distance_col = "distance",
#' use_default_benchmark_val = FALSE, col_start = "x", type_nldr = "UMAP", r2 = r2)
#'
#' @export
show_link_plots <- function(df_all, df_b, df_b_with_center_data, benchmark_value,
                             distance_df, distance_col,
                            use_default_benchmark_val = FALSE, col_start = "x",
                            type_nldr, r2) {

  num_highd_col <- df_all |>
    dplyr::select(starts_with(col_start)) |>
    NCOL()

  df_all <- df_all |>
    dplyr::mutate(type = "data")

  df_b <- df_b |>
    dplyr::filter(hb_id %in% df_b_with_center_data$hexID)

  ## Reorder the rows of df_b according to the hexID order in df_b_with_center_data
  df_b <- df_b[match(df_b_with_center_data$hexID, df_b$hb_id),] |>
    dplyr::select(-hb_id) |>
    dplyr::mutate(type = "model")

  df_exe <- dplyr::bind_rows(df_b, df_all)

  shared_df <- crosstalk::SharedData$new(df_exe)

  emb1 <- paste0(type_nldr, "1")
  emb2 <- paste0(type_nldr, "2")

  nldr_plt <- shared_df |>
    ggplot(aes_string(x = emb1, y = emb2)) +
    geom_point(alpha=0.5, colour="#000000", size = 0.5) +
    theme_linedraw() +
    theme(
      #aspect.ratio = 1,
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title = element_text(size = 7, hjust = 0.5, vjust = -0.5),
      panel.background = element_rect(fill = 'transparent',
                                      colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank()
    )

  nldr_plt <- ggplotly(nldr_plt, width = as.character(round(600/r2, 0)),
                              height = "600", tooltip = "none") |>
    style(unselected=list(marker=list(opacity=1))) |>
    highlight(on="plotly_selected", off="plotly_deselect") |>
    config(displayModeBar = FALSE)


  if(missing(benchmark_value)){

    if (isFALSE(use_default_benchmark_val)) {

      tr1 <- tri_bin_centroids(hex_df = df_b_with_center_data, x = "c_x", y = "c_y")
      tr_from_to_df <- gen_edges(tri_object = tr1)

      langevitour_output <- langevitour::langevitour(df_exe[1:num_highd_col],
                                                     lineFrom = tr_from_to_df$from,
                                                     lineTo = tr_from_to_df$to,
                                                     group = df_exe$type,
                                                     pointSize = append(rep(2, NROW(df_b)),
                                                                        rep(1, NROW(df_all))),
                                                     levelColors = c("#000000", "#33a02c"),
                                                     link=shared_df,
                                                     link_filter=FALSE)

    } else {

      benchmark_value <- find_lg_benchmark(distance_edges = distance_df,
                                           distance_col = distance_col)

      ## Set the maximum difference as the criteria
      distance_df_small_edges <- distance_df |>
        filter(!!as.name(distance_col) < benchmark_value)
      ## Since erase brushing is considered.

      langevitour_output <- langevitour::langevitour(df_exe[1:num_highd_col],
                               lineFrom = distance_df_small_edges$from,
                               lineTo = distance_df_small_edges$to,
                               group = df_exe$type,
                               pointSize = append(rep(2, NROW(df_b)),
                                                  rep(1, NROW(df_all))),
                               levelColors = c("#000000", "#33a02c"),
                               link=shared_df,
                               link_filter=FALSE)

    }

  } else {

    ## Check benchmark value is an accepted one
    if (benchmark_value < min(distance_df[[rlang::as_string(rlang::sym(distance_col))]])) {
      stop("Benchmark value to remove long edges is too small.")

    }

    if (benchmark_value > max(distance_df[[rlang::as_string(rlang::sym(distance_col))]])) {
      stop("Benchmark value to remove long edges is too large.")

    }

    if (isTRUE(use_default_benchmark_val)) {
      stop("Need to set `benchmark_value = NA`.")
    }

    ## Set the maximum difference as the criteria
    distance_df_small_edges <- distance_df |>
      dplyr::filter((!!as.name(distance_col)) < benchmark_value)
    ## Since erase brushing is considerd.

    langevitour_output <- langevitour::langevitour(df_exe[1:num_highd_col],
                             lineFrom = distance_df_small_edges$from,
                             lineTo = distance_df_small_edges$to,
                             group = df_exe$type,
                             pointSize = append(rep(2, NROW(df_b)),
                                                rep(1, NROW(df_all))),
                             levelColors = c("#000000", "#33a02c"),
                             link=shared_df,
                             link_filter=FALSE)

  }

  linked_plt <- crosstalk::bscols(
    nldr_plt,
    langevitour_output,
    widths = c(6, 6),
    device = "sm"
  )

  linked_plt

}
