#' Create a tibble with averaged high-dimensional data and
#' high-dimensional data, non-linear dimension reduction data
#'
#' This function combine the average values of high-dimensional data within each
#' hexagonal bin and high-dimensional data, non-linear dimension reduction data.
#'
#' @param highd_data A tibble that contains the high-dimensional data.
#' @param nldr_data A tibble that contains the non-linear dimension reduction data.
#' @param model_highd A tibble that contains the high-dimensional coordinates of bin centroids.
#' @param model_2d A tibble that contains hexagonal bin centroids in 2-D.
#'
#' @return A tibble with the average values of the high-dimensional data within
#' each hexagonal bin and high-dimensional data, non-linear dimension reduction data.
#'
#' @importFrom dplyr select mutate inner_join
#' @importFrom rsample starts_with
#'
#' @examples
#' comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap,
#' model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$model_2d)
#'
#' @export
comb_all_data_model <- function(highd_data, nldr_data, model_highd, model_2d) {

  df <- inner_join(highd_data, nldr_data, by = "ID")

  ### Define type column
  df <- df |>
    select(starts_with("x"), starts_with("emb")) |>
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

#' Visualise the model overlaid on high-dimensional data along with 2-D wireframe model.
#'
#' This function generates a LangeviTour visualisation based on different
#' conditions and input parameters with 2-D wireframe.
#'
#' @param point_data A tibble that contains the high-dimensional data, non-linear dimension reductions
#' and model in high-dimensions.
#' @param edge_data A tibble that contains the wireframe data (from and to).
#' @param point_colours A character vector that contains the colours of points in
#' the high-dimensional data and model.
#' @param point_sizes A numeric vector that contains the sizes of points in
#' the high-dimensional data and model.
#'
#'
#' @return A browsable HTML element.
#'
#' @importFrom dplyr mutate bind_rows filter select
#' @importFrom langevitour langevitour
#' @importFrom ggplot2 ggplot theme_linedraw aes_string theme element_rect element_text element_blank
#' @importFrom plotly plot_ly config highlight style layout
#'
#' @examples
#' df_exe <- comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap,
#' model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$model_2d)
#' edge_data <- scurve_model_obj$trimesh_data
#' if (interactive()) {
#'   show_link_plots(point_data = df_exe, edge_data = edge_data)
#' }
#'
#' @export
show_link_plots <- function(point_data, edge_data,
                            point_colours = c("#66B2CC", "#FF7755"),
                            point_sizes = c(0, 1)) {

  num_highd_col <- point_data |>
    dplyr::select(starts_with("x")) |>
    NCOL()

  df_all <- point_data |>
    dplyr::filter(type == "data") ## original dataset

  df_b <- point_data |>
    dplyr::filter(type == "model") ## High-d model

  shared_df <- crosstalk::SharedData$new(point_data)

  nldr_plt <- plot_ly(
    shared_df,
    x = ~emb1, y = ~emb2,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = point_colours[1],
      size = 3,
      opacity = 0.5
    ),
    hoverinfo = "none",
    width = 300,
    height = 300
  ) |>
    layout(
      xaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        linecolor = "black",
        mirror = TRUE
      ),
      yaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        linecolor = "black",
        mirror = TRUE
      ),
      margin = list(l = 20, r = 20, t = 20, b = 20),
      dragmode = "select"
    ) |>
    style(selected   = list(marker = list(opacity = 1)), unselected=list(marker=list(opacity=1))) |>
    highlight(on="plotly_selected", off="plotly_deselect") |>
    config(displayModeBar = FALSE)


  langevitour_output <- langevitour::langevitour(point_data[1:num_highd_col],
                                                 lineFrom = edge_data$from_reindexed,
                                                 lineTo = edge_data$to_reindexed,
                                                 group = point_data$type,
                                                 pointSize = append(rep(point_sizes[1], NROW(df_b)),
                                                                    rep(point_sizes[2], NROW(df_all))),
                                                 levelColors = point_colours,
                                                 link=shared_df,
                                                 linkFilter=FALSE,
                                                 height = "350px",
                                                 width = "350px")

  linked_plt <- crosstalk::bscols(htmltools::div(
    style = "display: grid; grid-template-columns: 1fr 1fr;
    gap: 0px;
    align-items: start;
    justify-items: center;
    margin: 0;
    padding: 0;",
    htmltools::div(style = 'margin: 0; padding: 0; height: 300px; width: 300px; text-align: center; align-items: center;',
                   htmltools::h4("2-D NLDR layout"), nldr_plt),
    htmltools::div(style = 'margin: 0; padding: 0; height: 350px; width: 300px; text-align: center;',
                   htmltools::h4("Tour view"), htmltools::div(
      style = "margin-top: 40px;",
      langevitour_output
    ))
  ),
  device = "xs")


  linked_plt

}

#' Create a tibble with averaged high-dimensional data and
#' high-dimensional data, non-linear dimension reduction data, model error data
#'
#' This function combine the average values of high-dimensional data within each
#' hexagonal bin and high-dimensional data, non-linear dimension reduction data,
#' model error data.
#'
#' @param highd_data A tibble that contains the high-dimensional data.
#' @param nldr_data A tibble that contains the non-linear dimension reduction data.
#' @param model_highd A tibble that contains the high-dimensional coordinates of bin centroids.
#' @param model_2d A tibble that contains hexagonal bin centroids in 2-D.
#' @param error_data A tibble that contains high-dimensional model error.
#'
#' @return A tibble with the average values of the high-dimensional data within
#' each hexagonal bin and high-dimensional data, non-linear dimension reduction data, model error.
#'
#' @importFrom dplyr select mutate inner_join
#' @importFrom rsample starts_with
#'
#' @examples
#' model_error <- augment(x = scurve_model_obj, highd_data = scurve)
#' comb_all_data_model_error(highd_data = scurve, nldr_data = scurve_umap,
#' model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$model_2d,
#' error_data = model_error)
#'
#' @export
comb_all_data_model_error <- function(highd_data, nldr_data, model_highd,
                                      model_2d, error_data) {

  df <- inner_join(highd_data, nldr_data, by = "ID")

  error_data <- error_data |>
    dplyr::mutate(sqrt_row_wise_total_error = sqrt(row_wise_total_error))

  # Compute density
  density_data <- density(error_data$sqrt_row_wise_total_error)
  density_df <- data.frame(x = density_data$x, y = density_data$y)

  # Add density values to the original dataset
  error_data <- error_data |>
    dplyr::mutate(density = approx(density_df$x, density_df$y,
                                   xout = sqrt_row_wise_total_error)$y)

  ### Define type column
  df <- df |>
    select(starts_with("x"), starts_with("emb")) |>
    mutate(type = "data") |> ## original dataset
    dplyr::mutate(sqrt_row_wise_total_error = error_data$sqrt_row_wise_total_error) |>
    dplyr::mutate(density = error_data$density)

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


#' Visualise the model overlaid on high-dimensional data along with 2-D wireframe model and error.
#'
#' This function generates a LangeviTour visualisation based on different
#' conditions and input parameters with 2-D wireframe.
#'
#' @param point_data A tibble that contains the high-dimensional data, no-linear dimension reductions
#' and model in high-dimensions.
#' @param edge_data A tibble that contains the wireframe data (from and to).
#' @param point_colours A character vector that contains the colours of points in
#' the high-dimensional data and model.
#' @param point_sizes A numeric vector that contains the sizes of points in
#' the high-dimensional data and model.
#'
#'
#' @return A browsable HTML element.
#'
#' @importFrom dplyr mutate bind_rows filter select
#' @importFrom langevitour langevitour
#' @importFrom ggplot2 ggplot theme_bw theme_linedraw aes aes_string theme element_rect element_text element_blank geom_point xlab ylab
#' @importFrom plotly plot_ly config highlight style layout
#' @examples
#' model_error <- augment(x = scurve_model_obj, highd_data = scurve)
#' df_exe <- comb_all_data_model_error(highd_data = scurve, nldr_data = scurve_umap,
#' model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$model_2d,
#' error_data = model_error)
#' edge_data <- scurve_model_obj$trimesh_data
#' if (interactive()) {
#'   show_error_link_plots(point_data = df_exe, edge_data = edge_data)
#' }
#'
#' @export
show_error_link_plots <- function(point_data, edge_data,
                                  point_colours = c("#66B2CC", "#FF7755"),
                                  point_sizes = c(0, 1)) {

  num_highd_col <- point_data |>
    dplyr::select(starts_with("x")) |>
    NCOL()

  df_all <- point_data |>
    dplyr::filter(type == "data") ## original dataset

  df_b <- point_data |>
    dplyr::filter(type == "model") ## High-d model

  shared_df <- crosstalk::SharedData$new(point_data)

  error_plt <- plot_ly(
    shared_df,
    x = ~sqrt_row_wise_total_error, y = ~density,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = point_colours[1],
      size = 3,
      opacity = 0.5
    ),
    hoverinfo = "none",
    width = 200,
    height = 200
  ) |>
    layout(
      xaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        linecolor = "black",
        mirror = TRUE
      ),
      yaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        linecolor = "black",
        mirror = TRUE
      ),
      margin = list(l = 20, r = 20, t = 20, b = 20),
      dragmode = "select"
    ) |>
    style(selected   = list(marker = list(opacity = 1)), unselected=list(marker=list(opacity=1))) |>
    highlight(on="plotly_selected", off="plotly_deselect") |>
    config(displayModeBar = FALSE)

  nldr_plt <- plot_ly(
    shared_df,
    x = ~emb1, y = ~emb2,
    type = "scatter",
    mode = "markers",
    marker = list(
      color = point_colours[1],
      size = 3,
      opacity = 0.5
    ),
    hoverinfo = "none",
    width = 200,
    height = 200
  ) |>
    layout(
      xaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        linecolor = "black",
        mirror = TRUE
      ),
      yaxis = list(
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        linecolor = "black",
        mirror = TRUE
      ),
      margin = list(l = 20, r = 20, t = 20, b = 20),
      dragmode = "select"
    ) |>
    style(selected   = list(marker = list(opacity = 1)), unselected=list(marker=list(opacity=1))) |>
    highlight(on="plotly_selected", off="plotly_deselect") |>
    config(displayModeBar = FALSE)


  langevitour_output <- langevitour::langevitour(point_data[1:num_highd_col],
                                                 lineFrom = edge_data$from_reindexed,
                                                 lineTo = edge_data$to_reindexed,
                                                 group = point_data$type,
                                                 pointSize = append(rep(point_sizes[1], NROW(df_b)),
                                                                    rep(point_sizes[2], NROW(df_all))),
                                                 levelColors = point_colours,
                                                 link=shared_df,
                                                 linkFilter=FALSE,
                                                 width = "300px",
                                                 height = "300px")

  linked_plt <- crosstalk::bscols(htmltools::div(style = "display: grid; grid-template-columns: 1fr 1fr 1fr;
    gap: 0px;
    align-items: start;
    justify-items: center;
    margin: 0;
    padding: 0;",
    htmltools::div(style = 'margin: 0; padding: 0; height: 400px; width: 200px; text-align: center; align-items: center;',
                   htmltools::h5("Distribution of residuals"), htmltools::div(
                     style = "margin-top: -10px;",
                     error_plt
                   )),
    htmltools::div(style = 'margin: 0; padding: 0; height: 400px; width: 200px; text-align: center; align-items: center;',
                   htmltools::h5("2-D NLDR layout"), nldr_plt),
    htmltools::div(style = 'margin: 0; padding: 0; height: 400px; width: 250px; text-align: center; align-items: center;',
                   htmltools::h5("Tour view"), htmltools::div(
                     style = "margin-top: 40px;",
                     langevitour_output
                   ))
  ))

  linked_plt

}

