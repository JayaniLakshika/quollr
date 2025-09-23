#' Construct the 2-D model and lift into high-dimensions
#'
#' This function fits a high-dimensional model using hexagonal bins and provides options
#' to customize the modeling process, including the choice of bin centroids or bin means,
#' removal of low-density hexagons, and averaging of high-dimensional data.
#'
#' @param highd_data A tibble that contains the high-dimensional data with a unique identifier.
#' @param nldr_data A tibble that contains the embedding with a unique identifier.
#' @param b1 (default: 4) A numeric value representing the number of bins along the x axis.
#' @param q (default: 0.1) A numeric value representing the buffer amount as proportion of data range.
#' @param hd_thresh (default: 0) A numeric value using to filter high-density hexagons.
#'
#' @return A list containing a list of a tibble contains scaled first and second columns
#' of NLDR data, and numeric vectors representing the limits of the original NLDR data (\code{nldr_scaled_obj}),
#' a object that contains hexagonal binning information (\code{hb_obj}),
#' a tibble with high-dimensional model (\code{model_highd}) and a tibble containing
#' hexagonal bin centroids in 2-D (\code{model_2d}), and
#' a tibble that contains the edge information (\code{trimesh_data}).
#'
#' @importFrom dplyr bind_cols filter select between
#' @importFrom stats quantile
#'
#' @examples
#' fit_highd_model(highd_data = scurve, nldr_data = scurve_umap, b1 = 30,
#' q = 0.1, hd_thresh = 0)
#'
#' @export
fit_highd_model <- function(highd_data, nldr_data, b1 = 30, q = 0.1,
                            hd_thresh = 0) {

  ## To pre-process the data
  nldr_scaled_obj <- gen_scaled_data(nldr_data = nldr_data)

  ## Obtain the hexbin object
  hb_obj <- hex_binning(nldr_scaled_obj = nldr_scaled_obj, b1 = b1, q = q)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts

  ## To extract all bin centroids with bin counts
  df_bin_centroids <- merge_hexbin_centroids(centroids_data = all_centroids_df,
                                               counts_data = counts_df)

  ## Wireframe
  tr_object <- tri_bin_centroids(centroids_data = df_bin_centroids)
  trimesh_data <- gen_edges(tri_object = tr_object, a1 = hb_obj$a1) |>
    dplyr::filter(from_count > hd_thresh,
                  to_count > hd_thresh)

  ## Update the edge indexes to start from 1
  trimesh_data <- update_trimesh_index(trimesh_data)

  ## averaged high-D data
  nldr_df_with_hex_id <- hb_obj$data_hb_id
  model_highd <- avg_highd_data(highd_data = highd_data,
                                scaled_nldr_hexid = nldr_df_with_hex_id)

  ## To extract high-densed bins
  model_2d <- df_bin_centroids |>
    dplyr::filter(n_h > hd_thresh)

  model_highd <- model_highd |>
    dplyr::filter(h %in% model_2d$h)

  cli::cli_alert_success("Model generated successfully!!!")

  ## Store as S3 object
  res <- list(
    nldr_scaled_obj = nldr_scaled_obj,
    hb_obj = hb_obj,
    model_highd = model_highd,
    model_2d = model_2d,
    trimesh_data = trimesh_data
  )

  class(res) <- "highd_vis_model"

  return(res)

}

