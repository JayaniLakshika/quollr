#' Create a dataframe with averaged high-dimensional data and
#' high-dimensional data, non-linear dimension reduction data
#'
#' This function combine the average values of high-dimensional data within each
#' hexagonal bin and high-dimensional data, non-linear dimension reduction data.
#'
#' @param highd_data A tibble that contains the high-dimensional data.
#' @param nldr_data A tibble that contains the non-linear dimension reduction data.
#' @param model_highd A tibble that contains the high-dimensional coordinates of bin centroids.
#' @param model_2d The dataset with hexagonal bin centroids.
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
    filter(hexID %in% model_2d$hexID) |>
    mutate(type = "model") ## Data with summarized mean

  ## Reorder the rows of df_b according to the hexID order in model_2d
  df_b <- df_b[match(model_2d$hexID, df_b$hexID),] |>
    select(-hexID)

  df_exe <- bind_rows(df_b, df)

  df_exe

}

#' Visualize the model overlaid on high-dimensional data along with 2D wireframe model.
#'
#' This function generates a LangeviTour visualization based on different
#' conditions and input parameters with 2D wireframe.
#'
#' @param point_df A tibble that contains the high-dimensional data, non-linear dimension reductions
#' and model in high-dimensions.
#' @param edge_df A tibble that contains the wireframe data (from and to).
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
#' df_exe <- comb_all_data_model(highd_data = scurve, nldr_data = scurve_umap,
#' model_highd = scurve_model_obj$model_highd, model_2d = scurve_model_obj$model_2d)
#' edge_data <- scurve_model_obj$trimesh_data
#' show_link_plots(point_df = df_exe, edge_df = edge_data)
#'
#' @export
show_link_plots <- function(point_df, edge_df) {

  num_highd_col <- point_df |>
    dplyr::select(starts_with("x")) |>
    NCOL()

  df_all <- point_df |>
    dplyr::filter(type == "data") ## original dataset

  df_b <- point_df |>
    dplyr::filter(type == "model") ## High-d model

  shared_df <- crosstalk::SharedData$new(point_df)

  nldr_plt <- shared_df |>
    ggplot(aes(x = emb1, y = emb2)) +
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

  nldr_plt <- ggplotly(nldr_plt, width = "600",
                              height = "600", tooltip = "none") |>
    style(unselected=list(marker=list(opacity=1))) |>
    highlight(on="plotly_selected", off="plotly_deselect") |>
    config(displayModeBar = FALSE)


  langevitour_output <- langevitour::langevitour(point_df[1:num_highd_col],
                                                 lineFrom = edge_df$from,
                                                 lineTo = edge_df$to,
                                                 group = point_df$type,
                                                 pointSize = append(rep(2, NROW(df_b)),
                                                                    rep(1, NROW(df_all))),
                                                 levelColors = c("#000000", "#33a02c"),
                                                 link=shared_df,
                                                 linkFilter=FALSE)

  linked_plt <- crosstalk::bscols(
    htmltools::div(style="display: grid; grid-template-columns: 1fr 1fr;",
                   nldr_plt,
                   langevitour_output),
    device = "sm"
  )

  linked_plt

}
