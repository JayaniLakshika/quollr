# Center the data by subtracting the mean of each column
center_data <- function(data) {
  apply(data, 2, function(col) col - mean(col))
}

# Function to scale data manually
scale_data_manual <- function(data, type_col) {
  # Step 1: Center the data (mean 0)
  data_centered <- center_data(data |> select(-all_of(type_col)))

  # Step 2: Calculate the standard deviation of each dimension
  sds <- apply(data_centered, 2, sd)

  # Step 3: Scale each dimension to have the range [0, 1]
  data_scaled <- apply(data_centered, 2, function(col) col / max(abs(col)))

  # Step 4: Scale dimensions according to their variation
  # The dimension with the highest standard deviation is scaled to [-1, 1]
  # Other dimensions are scaled to smaller ranges based on their standard deviations
  max_sd <- max(sds)

  # Normalize the standard deviations to get scaling factors
  scaling_factors <- sds / max_sd

  for (i in seq_along(scaling_factors)) {
    data_scaled[, i] <- data_scaled[, i] * scaling_factors[i]
  }

  # Combine the scaled data with the 'type' column and return as a tibble
  data_scaled <- as_tibble(data_scaled) %>%
    mutate(!!type_col := data[[type_col]])

  return(data_scaled)
}

get_projection <- function(projection, proj_scale, scaled_data,
                           scaled_data_model, distance_df_small_edges,
                           axis_param) {

  projection_scaled <- projection * proj_scale
  projected <- as.matrix(scaled_data) %*% projection_scaled

  projected_df <- projected |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::rename(c("proj1" = "...1",
                    "proj2" = "...2")) |>
    dplyr::mutate(ID = dplyr::row_number())

  projected_model <- as.matrix(scaled_data_model) %*% projection_scaled

  projected_model_df <- projected_model |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::rename(c("proj1" = "...1",
                    "proj2" = "...2")) |>
    dplyr::mutate(ID = dplyr::row_number())

  model_df <- dplyr::left_join(
    distance_df_small_edges |> select(-distance),
    projected_model_df,
    by = c("from" = "ID"))

  names(model_df)[3:NCOL(model_df)] <- paste0(names(projected_model_df)[-NCOL(projected_model_df)], "_from")

  model_df <- dplyr::left_join(model_df, projected_model_df, by = c("to" = "ID"))
  names(model_df)[(2 + NCOL(projected_model_df)):NCOL(model_df)] <- paste0(names(projected_model_df)[-NCOL(projected_model_df)], "_to")

  limits <- axis_param$limits
  axis_scaled <- axis_param$axis_scaled
  axis_pos_x <- axis_param$axis_pos_x
  axis_pos_y <- axis_param$axis_pos_y
  threshold <- axis_param$threshold

  axes_obj <- gen_axes(
    proj = projection * axis_scaled,
    limits = limits,
    axis_pos_x = axis_pos_x,
    axis_pos_y = axis_pos_y,
    axis_labels = names(scaled_data),
    threshold = threshold)

  axes <- axes_obj$axes
  circle <- axes_obj$circle

  return(list(projected_df = projected_df,
              model_df = model_df,
              axes = axes,
              circle = circle))

}

gen_axes <- function(proj, limits = 1, axis_pos_x = NULL, axis_pos_y = NULL,
                     axis_labels, threshold = 0) {

  axis_scale <- limits/6

  if (is.null(axis_pos_x)) {

    axis_pos_x <- -2/3 * limits

  }

  if (is.null(axis_pos_y)) {

    axis_pos_y <- -2/3 * limits

  }

  adj <- function(x, axis_pos) axis_pos + x * axis_scale
  axes <- data.frame(x1 = adj(0, axis_pos_x),
                     y1 = adj(0, axis_pos_y),
                     x2 = adj(proj[, 1], axis_pos_x),
                     y2 = adj(proj[, 2], axis_pos_y))

  rownames(axes) <- axis_labels

  ## To remove axes
  axes <- axes |>
    mutate(distance = sqrt((x2 - x1)^2 + (y2 - y1)^2)) |>
    filter(distance >= threshold)

  theta <- seq(0, 2 * pi, length = 50)
  circle <- data.frame(c1 = adj(cos(theta), axis_pos_x),
                       c2 = adj(sin(theta), axis_pos_y))

  return(list(axes = axes, circle = circle))

}


plot_proj <- function(projected_df, model_df, axes, circle,
                      point_param = c(1.5, 0.5, "#000000"), # size, alpha, color
                      line_param = c(0.5, 0.5, "#000000"), #linewidth, alpha
                      plot_limits, title, cex = 2,
                      position = c(0.92, 0.92),
                      axis_text_size = 3,
                      is_category = FALSE) {

  if(is_category == FALSE) {

    initial_plot <- projected_df |>
      ggplot(
        aes(
          x = proj1,
          y = proj2)) +
      geom_segment(
        data = model_df,
        aes(
          x = proj1_from,
          y = proj2_from,
          xend = proj1_to,
          yend = proj2_to),
        color = line_param[3],
        linewidth = as.numeric(line_param[1]),
        alpha = as.numeric(line_param[2])) +
      geom_point(
        size = as.numeric(point_param[1]),
        alpha = as.numeric(point_param[2]),
        color = point_param[3])

  } else {

    initial_plot <- projected_df |>
      ggplot(
        aes(
          x = proj1,
          y = proj2,
          colour = cluster)) +
      geom_segment(
        data = model_df,
        aes(
          x = proj1_from,
          y = proj2_from,
          xend = proj1_to,
          yend = proj2_to),
        color = line_param[3],
        linewidth = as.numeric(line_param[1]),
        alpha = as.numeric(line_param[2])) +
      geom_point(
        size = as.numeric(point_param[1]),
        alpha = as.numeric(point_param[2]))

  }

  initial_plot <- initial_plot +
    geom_segment(
      data=axes,
      aes(x=x1, y=y1, xend=x2, yend=y2),
      colour="grey70") +
    geom_text(
      data=axes,
      aes(x=x2, y=y2),
      label=rownames(axes),
      colour="grey50",
      size = axis_text_size) +
    geom_path(
      data=circle,
      aes(x=c1, y=c2), colour="grey70") +
    xlim(plot_limits) +
    ylim(plot_limits) +
    interior_annotation(title, position, cex = cex)

  initial_plot

}


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
#' umap_data_with_hb_id <- s_curve_obj$s_curve_umap_hb_obj$data_hb_id
#' df_all <- dplyr::bind_cols(s_curve_noise_training |> dplyr::select(-ID),
#' umap_data_with_hb_id)
#' df_bin_centroids <- s_curve_obj$s_curve_umap_model_distance_df$df_bin_centroids
#' df_bin <- s_curve_obj$s_curve_umap_model_distance_df$df_bin
#' distance_df <- s_curve_obj$s_curve_umap_model_distance_df
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
    geom_segment(data = model_df, aes(x = proj1_from, y = proj2_from, xend = proj1_to, yend = proj2_to), color = "#33a02c", alpha = 0.5) +
    scale_color_manual(values = c("#000000", "#33a02c"))

  return(proj_plot)

}
