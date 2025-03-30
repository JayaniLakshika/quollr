#' Construct the 2D model and lift into high-D
#'
#' This function fits a high-dimensional model using hexagonal bins and provides options
#' to customize the modeling process, including the choice of bin centroids or bin means,
#' removal of low-density hexagons, and averaging of high-dimensional data.
#'
#' @param highd_data A tibble that contains the training high-dimensional data.
#' @param nldr_data A tibble that contains embedding with a unique identifier.
#' @param bin1 Number of bins along the x axis.
#' @param r2 The ratio of the ranges of the original embedding components.
#' @param q The buffer amount as proportion of data range.
#'
#' @return A list containing the data frame with high-dimensional coordinates
#' for 2D bin centroids (\code{df_bin}) and the data frame containing
#' information about hexagonal bin centroids (\code{df_bin_centroids}) in 2D.
#'
#' @importFrom dplyr bind_cols filter select between
#' @importFrom stats quantile
#'
#' @examples
#' scurve_umap_scaled_obj <- s_curve_obj$s_curve_umap_scaled_obj
#' lim1 <- scurve_umap_scaled_obj$lim1
#' lim2 <- scurve_umap_scaled_obj$lim2
#' r2 <- diff(lim2)/diff(lim1)
#' fit_highd_model(highd_data = s_curve_noise_training,
#' nldr_data = s_curve_noise_umap_scaled, bin1 = 4, r2 = r2)
#'
#' @export
fit_highd_model <- function(highd_data, nldr_data, bin1 = 4, r2, q = 0.1,
                            is_bin_centroid = TRUE) {

  ## Obtain the hexbin object
  hb_obj <- hex_binning(data = nldr_data, bin1 = bin1, r2 = r2, q = q)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  nldr_df_with_hex_id <- hb_obj$data_hb_id

  ## Do you need to use bin centroids or bin means?
  if (isTRUE(is_bin_centroid)) {
    ## For bin centroids
    df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                                 counts_df = counts_df) |>
      filter(drop_empty == FALSE)

  } else {
    ## For bin means
    df_bin_centroids <- extract_hexbin_mean(data_hb = nldr_df_with_hex_id,
                                            counts_df = counts_df,
                                            centroids_df = all_centroids_df) |>
      filter(drop_empty == FALSE)

  }

  ## To generate a data set with high-D and 2D training data
  df_all <- bind_cols(highd_data |> select(-ID), nldr_df_with_hex_id)

  ## averaged high-D data
  df_bin <- avg_highd_data(data = df_all)

  ## high-D model only contains the bins in 2D
  df_bin <- df_bin |>
    filter(hb_id %in% df_bin_centroids$hexID)

  cli::cli_alert_success("Model generated successfully! 🎉")

  return(list(df_bin = df_bin, df_bin_centroids = df_bin_centroids))

}

