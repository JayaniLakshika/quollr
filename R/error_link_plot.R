#' Visualize the model overlaid on high-dimensional data along with 2D wireframe model and error.
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
#' @importFrom ggplot2 ggplot theme_bw theme_linedraw aes aes_string theme element_rect element_text element_blank geom_point xlab ylab
#' @importFrom plotly ggplotly config highlight style
#' @examples
#' umap_data_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID),
#' umap_data_with_hb_id)
#' df_bin_centroids <- s_curve_obj$s_curve_umap_model_obj$df_bin_centroids
#' df_bin <- s_curve_obj$s_curve_umap_model_obj$df_bin
#' distance_df <- s_curve_obj$s_curve_umap_model_distance_df
#' show_error_link_plots(df_all = df_all, df_b = df_bin, df_b_with_center_data = df_bin_centroids,
#' benchmark_value = 0.8, distance = distance_df, distance_col = "distance",
#' use_default_benchmark_val = FALSE, col_start = "x", type_nldr = "UMAP", r2 = r2)
#'
#' @export
show_error_link_plots <- function(df_all, df_b, df_b_with_center_data, benchmark_value,
                                  distance_df, distance_col,
                                  use_default_benchmark_val = FALSE, col_start = "x",
                                  type_nldr, r2) {



  num_highd_col <- df_all |>
    dplyr::select(starts_with(col_start)) |>
    NCOL()

  ## Compute error
  error_df <- augment(
    df_bin_centroids = df_b_with_center_data,
    df_bin = df_b,
    training_data = df_all |>
      dplyr::select(ID, starts_with(col_start)),
    newdata = NULL,
    type_NLDR = type_nldr,
    col_start = col_start)

  error_df <- error_df |>
    dplyr::mutate(sqrt_row_wise_total_error = sqrt(row_wise_total_error))

  # Compute density
  density_data <- density(error_df$sqrt_row_wise_total_error)
  density_df <- data.frame(x = density_data$x, y = density_data$y)

  # Add density values to the original dataset
  error_df <- error_df |>
    dplyr::mutate(density = approx(density_df$x, density_df$y, xout = sqrt_row_wise_total_error)$y)

  # error_plot <-
  #   ggplot(error_df) +
  #   geom_histogram(aes(x=sqrt_row_wise_total_error, y=..density..)) +
  #   geom_density(aes(x=sqrt_row_wise_total_error, y=..density..), colour="red") +
  #   xlab(expression(e[hj])) +
  #   ylab("") +
  #   theme_bw()

  df_all <- df_all |>
    dplyr::mutate(type = "data") |>
    dplyr::mutate(sqrt_row_wise_total_error = error_df$sqrt_row_wise_total_error) |>
    dplyr::mutate(density = error_df$density)

  df_b <- df_b |>
    dplyr::filter(hb_id %in% df_b_with_center_data$hexID)

  ## Reorder the rows of df_b according to the hexID order in df_b_with_center_data
  df_b <- df_b[match(df_b_with_center_data$hexID, df_b$hb_id),] |>
    dplyr::select(-hb_id) |>
    dplyr::mutate(type = "model")

  df_exe <- dplyr::bind_rows(df_b, df_all)

  shared_df <- crosstalk::SharedData$new(df_exe)

  error_plt <- shared_df |>
    ggplot(aes(x=sqrt_row_wise_total_error, y = density)) +
    geom_point() +
    xlab(expression(e[hj])) +
    ylab("") +
    theme_bw() +
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

  error_plt <- ggplotly(error_plt, width = "400",
                        height = "400", tooltip = "none") |>
    style(unselected=list(marker=list(opacity=1))) |>
    highlight(on="plotly_selected", off="plotly_deselect") |>
    config(displayModeBar = FALSE)

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

  nldr_plt <- ggplotly(nldr_plt, width = "400",
                       height = "400", tooltip = "none") |>
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
                                                     link_filter=FALSE,
                                                     width = "450",
                                                     height = "450")

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
                                                     link_filter=FALSE,
                                                     width = "450",
                                                     height = "450")

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
                                                   link_filter=FALSE,
                                                   width = "450",
                                                   height = "450")

  }

  # Create a table widget
  datatableWidget <- DT::datatable(
    shared_df,
    rownames = FALSE, extensions = "Buttons",
    options = list(paging = TRUE,
                   scrollX = TRUE,
                   searching = TRUE,
                   ordering = TRUE,
                   dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'excel', 'pdf'),
                   pageLength = 10,
                   lengthMenu = c(3, 5, 10)))

  linked_plt <- crosstalk::bscols(
    htmltools::div(
    htmltools::div(style="display: grid; grid-template-columns: 1fr 1fr 1fr;",
        error_plt,
        nldr_plt,
        langevitour_output),
    datatableWidget, style = "padding-left: 50px;"),
    device = "sm"
  )

  linked_plt

}

