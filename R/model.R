#' Construct the 2D model and lift into high-D
#'
#' This function fits a high-dimensional model using hexagonal bins and provides options
#' to customize the modeling process, including the choice of bin centroids or bin means,
#' removal of low-density hexagons, and averaging of high-dimensional data.
#'
#' @param training_data A data frame containing the training high-dimensional data.
#' @param nldr_df_with_id A data frame containing 2D embeddings with a unique identifier.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
#' @param num_bins_x Number of bins along the x-axis.
#' @param num_bins_y Number of bins along the y-axis.
#' @param x_start Starting point along the x-axis for hexagonal binning.
#' @param y_start Starting point along the y-axis for hexagonal binning.
#' @param buffer_x The buffer size along the x-axis.
#' @param buffer_y The buffer size along the y-axis.
#' @param hex_size A numeric value that initializes the radius of the outer circle
#' surrounding the hexagon.
#' @param is_rm_lwd_hex Logical, indicating whether to remove low-density hexagons
#' (default is FALSE).
#' @param benchmark_to_rm_lwd_hex The benchmark value to remove low-density hexagons.
#' @param col_start_2d The text prefix for columns in the 2D embedding data.
#' @param col_start_highd The text prefix for columns in the high-dimensional data.
#'
#' @return A list containing the data frame with high-dimensional coordinates
#' for 2D bin centroids (\code{df_bin}) and the data frame containing
#' information about hexagonal bin centroids (\code{df_bin_centroids}) in 2D.
#'
#' @examples
#' fit_highd_model(training_data = s_curve_noise_training, x = "UMAP1", y = "UMAP2",
#' nldr_df_with_id = s_curve_noise_umap_scaled, col_start_2d = "UMAP", col_start_highd = "x")
#'
#' @export
fit_highd_model <- function(training_data, nldr_df_with_id, x, y, num_bins_x = NA,
                      num_bins_y = NA, x_start = NA, y_start = NA,
                      buffer_x = NA, buffer_y = NA,  hex_size = NA,
                      is_rm_lwd_hex = FALSE, benchmark_to_rm_lwd_hex = NA,
                      col_start_2d, col_start_highd) {

  ## If number of bins along the x-axis and/or y-axis is not given
  if (is.na(num_bins_x) | is.na(num_bins_y)) {
    ## compute the number of bins along the x-axis
    bin_list <- calc_bins(data = nldr_df_with_id, x = x, y = y, hex_size = hex_size,
                          buffer_x = buffer_x, buffer_y = buffer_y)
    num_bins_x <- bin_list$num_x
    num_bins_y <- bin_list$num_y
  }

  ## Obtain the hexbin object
  hb_obj <- hex_binning(data = nldr_df_with_id, x = x, y = y, num_bins_x = num_bins_x,
                        num_bins_y = num_bins_y, x_start = x_start,
                        y_start = y_start, buffer_x = buffer_x,
                        buffer_y = buffer_y, hex_size = hex_size,
                        col_start = col_start_2d)

  all_centroids_df <- as.data.frame(do.call(cbind, hb_obj$centroids))
  counts_df <- as.data.frame(do.call(cbind, hb_obj$std_cts))
  nldr_df_with_hex_id <- as.data.frame(do.call(cbind, hb_obj$data_hb_id))

  ## To obtain bin centroids
  df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                               counts_df = counts_df)

  if (isFALSE(is_rm_lwd_hex)) {
    if (!is.na(benchmark_to_rm_lwd_hex)) {
      stop("Need to initialise `is_rm_lwd_hex = TRUE`.")
    }

  }


  ## Do you need to remove low density hexagons?
  if (isTRUE(is_rm_lwd_hex)) {

    ## if the benchmark value to remove low density hexagons is not provided
    if (is.na(benchmark_to_rm_lwd_hex)) {
      ## first quartile used as the default
      benchmark_to_rm_lwd_hex <- stats::quantile(df_bin_centroids$std_counts,
                                                 probs = c(0,0.25,0.5,0.75,1))[2]
    }

    ## Check the benchmark value pass by the function is an acceptable one
    if (benchmark_to_rm_lwd_hex < min(df_bin_centroids$std_counts)) {
      stop("Benchmark value to remove low density hexagons is too small.")
    }

    if (benchmark_to_rm_lwd_hex > max(df_bin_centroids$std_counts)) {
      stop("Benchmark value to remove low density hexagons is too large.")
    }

    ## To identify low density hexagons
    df_bin_centroids_low <- df_bin_centroids |>
      dplyr::filter(std_counts <= benchmark_to_rm_lwd_hex)

    ## To identify low-density hexagons needed to remove by investigating neighbouring mean density
    identify_rm_bins <- find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                          num_bins_x = num_bins_x,
                                          df_bin_centroids_low = df_bin_centroids_low)

    ## To remove low-density hexagons
    df_bin_centroids <- df_bin_centroids |>
      filter(!(hexID %in% identify_rm_bins))

  }

  ## To generate a data set with high-D and 2D training data
  df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), nldr_df_with_hex_id)

  ## averaged high-D data
  df_bin <- avg_highd_data(data = df_all, col_start = col_start_highd)

  ## high-D model only contains the bins in 2D
  df_bin <- df_bin |>
    dplyr::filter(hb_id %in% df_bin_centroids$hexID)

  return(list(df_bin = df_bin, df_bin_centroids = df_bin_centroids))

}
