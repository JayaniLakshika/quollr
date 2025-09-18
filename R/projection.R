#' Generate Axes for Projection
#'
#' @param proj A projection matrix or data frame.
#' @param limits Numeric value specifying axis limits (default: 1).
#' @param axis_pos_x Optional numeric value for x-axis position.
#' @param axis_pos_y Optional numeric value for y-axis position.
#' @param axis_labels A vector of axis labels.
#' @param threshold A numeric threshold value (default: 0).
#'
#' @return A modified projection with added axis elements.
#' @export
#'
#' @examples
#' projection_df <- cbind(
#' c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
#' c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))
#'
#' gen_axes(proj = projection_df, axis_labels = paste0("x", 1:7))
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

#' Compute Projection for High-Dimensional Data
#'
#' @param projection A matrix or data frame representing the projection.
#' @param proj_scale Scaling factor for the projection.
#' @param highd_data A data frame or matrix of high-dimensional data.
#' @param model_highd A model object or function used for high-dimensional transformation.
#' @param trimesh_data A data frame defining transformation from one space to another.
#' @param axis_param A list of parameters for axis configuration.
#'
#' @return A data frame or matrix with the transformed projection.
#' @export
#'
#' @examples
#' projection_df <- cbind(
#' c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
#' c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))
#'
#' df_bin <- scurve_model_obj$model_highd
#' edge_data <- scurve_model_obj$trimesh_data
#'
#' get_projection(projection = projection_df, proj_scale = 1,
#' highd_data = scurve, model_highd = df_bin,
#' trimesh_data = edge_data,
#' axis_param = list(limits = 1, axis_scaled = 3, axis_pos_x = -0.72,
#' axis_pos_y = -0.72,threshold = 0.09))
#'
get_projection <- function(projection, proj_scale, highd_data, model_highd,
                           trimesh_data, axis_param) {

  highd_data <- highd_data |>
    dplyr::select(tidyselect::starts_with("x"))

  projection_scaled <- projection * proj_scale
  projected <- as.matrix(highd_data) %*% projection_scaled

  projected_df <- projected |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::rename(c("proj1" = "...1",
                    "proj2" = "...2")) |>
    dplyr::mutate(ID = dplyr::row_number())

  model_highd <- model_highd |>
    dplyr::select(tidyselect::starts_with("x"))

  projected_model <- as.matrix(model_highd) %*% projection_scaled

  projected_model_df <- projected_model |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::rename(c("proj1" = "...1",
                    "proj2" = "...2")) |>
    dplyr::mutate(ID = dplyr::row_number())

  model_df <- dplyr::left_join(
    trimesh_data,
    projected_model_df,
    by = c("from_reindexed" = "ID"))

  names(model_df)[(length(names(model_df))-1):length(names(model_df))] <- paste0(names(projected_model_df)[-NCOL(projected_model_df)], "_from")

  model_df <- dplyr::left_join(model_df, projected_model_df, by = c("to_reindexed" = "ID"))
  names(model_df)[(length(names(model_df))-1):length(names(model_df))] <- paste0(names(projected_model_df)[-NCOL(projected_model_df)], "_to")

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
    axis_labels = names(highd_data),
    threshold = threshold)

  axes <- axes_obj$axes
  circle <- axes_obj$circle

  return(list(projected_df = projected_df,
              model_df = model_df,
              axes = axes,
              circle = circle))

}


#' Plot Projected Data with Axes and Circles
#'
#' @param proj_obj An object contains a tibble containing the projected data,
#' a tibble containing the model reference data, a list specifying the axes details,
#' and a list defining circle parameters.
#' @param point_param A vector specifying point size, alpha, and color (default: c(1, 0.3, "#66B2CC")).
#' @param line_param A vector specifying line width, alpha, and color (default: c(0.5, 0.5, "#000000")).
#' @param plot_limits Limits for the plot axes.
#' @param axis_text_size Size of axis text (default: 3).
#' @param is_category Logical indicating if the data is categorical (default: FALSE).
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot geom_point geom_segment geom_text geom_path aes xlim ylim
#'
#' @export
#'
#' @examples
#' projection_df <- cbind(
#' c(-0.17353,-0.02906,0.19857,0.00037,0.00131,-0.05019,0.03371),
#' c(-0.10551,0.14829,-0.02063,0.02658,-0.03150,0.19698,0.00044))
#'
#' df_bin <- scurve_model_obj$model_highd
#' edge_data <- scurve_model_obj$trimesh_data
#'
#' proj_obj1 <- get_projection(projection = projection_df, proj_scale = 1,
#' highd_data = scurve, model_highd = df_bin,
#' trimesh_data = edge_data,
#' axis_param = list(limits = 1, axis_scaled = 3, axis_pos_x = -0.72,
#' axis_pos_y = -0.72, threshold = 0.09))
#'
#' plot_proj(proj_obj = proj_obj1, plot_limits = c(-1, 1))
plot_proj <- function(proj_obj,
                      point_param = c(1, 0.3, "#66B2CC"), # size, alpha, color
                      line_param = c(0.5, 0.5, "#000000"), #linewidth, alpha
                      plot_limits,
                      axis_text_size = 3,
                      is_category = FALSE) {

    projected_df <- proj_obj$projected_df
    model_df <- proj_obj$model_df
    axes <- proj_obj$axes
    circle <- proj_obj$circle

  if(is_category == FALSE) {

    initial_plot <- projected_df |>
      ggplot(
        aes(
          x = proj1,
          y = proj2)) +
      geom_point(
        size = as.numeric(point_param[1]),
        alpha = as.numeric(point_param[2]),
        color = point_param[3]) +
      geom_segment(
          data = model_df,
          aes(
            x = proj1_from,
            y = proj2_from,
            xend = proj1_to,
            yend = proj2_to),
          color = line_param[3],
          linewidth = as.numeric(line_param[1]),
          alpha = as.numeric(line_param[2]))

  } else {

    projected_df <- projected_df |>
      dplyr::mutate(cluster = proj_obj$cluster)

    initial_plot <- projected_df |>
      ggplot(
        aes(
          x = proj1,
          y = proj2,
          colour = cluster)) +
      geom_point(
        size = as.numeric(point_param[1]),
        alpha = as.numeric(point_param[2])) +
      geom_segment(
        data = model_df,
        aes(
          x = proj1_from,
          y = proj2_from,
          xend = proj1_to,
          yend = proj2_to),
        color = line_param[3],
        linewidth = as.numeric(line_param[1]),
        alpha = as.numeric(line_param[2]))

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
    theme(
      aspect.ratio = 1
    )

  initial_plot

}
