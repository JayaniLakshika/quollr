#' Construct the 2D model and lift into high-D
#'
#' This function fits a high-dimensional model using hexagonal bins and provides options
#' to customize the modeling process, including the choice of bin centroids or bin means,
#' removal of low-density hexagons, and averaging of high-dimensional data.
#'
#' @param training_data A tibble that contains the training high-dimensional data.
#' @param emb_df A tibble that contains embedding with a unique identifier.
#' @param bin1 Number of bins along the x axis.
#' @param q The buffer amount as proportion of data range 0.05-0.1.
#' @param r2 The ratio of the ranges of the original embedding components.
#' @param is_bin_centroid Logical, indicating whether to use bin centroids (default is TRUE).
#' @param is_rm_lwd_hex Logical, indicating whether to remove low-density hexagons
#' (default is FALSE).
#' @param benchmark_to_rm_lwd_hex The benchmark value to remove low-density hexagons.
#' @param col_start_highd The text prefix for columns in the high-dimensional data.
#'
#' @return A list containing the data frame with high-dimensional coordinates
#' for 2D bin centroids (\code{df_bin}) and the data frame containing
#' information about hexagonal bin centroids (\code{df_bin_centroids}) in 2D.
#'
#' @importFrom dplyr bind_cols filter select between
#' @importFrom stats quantile
#'
#' @examples
#' r2 <- diff(range(s_curve_noise_umap$UMAP2))/diff(range(s_curve_noise_umap$UMAP1))
#' fit_highd_model(training_data = s_curve_noise_training,
#' emb_df = s_curve_noise_umap_scaled, bin1 = 3, r2 = r2,
#' col_start_highd = "x")
#'
#' @export
fit_highd_model <- function(training_data, emb_df, bin1 = 2, q = 0.1,
                            r2, is_bin_centroid = TRUE,
                            is_rm_lwd_hex = FALSE, benchmark_to_rm_lwd_hex,
                            col_start_highd = "x") {

  ## Obtain the hexbin object
  hb_obj <- hex_binning(data = emb_df, bin1 = bin1, q = q, r2 = r2)

  all_centroids_df <- hb_obj$centroids
  counts_df <- hb_obj$std_cts
  nldr_df_with_hex_id <- hb_obj$data_hb_id

  ## Do you need to use bin centroids or bin means?
  if (isTRUE(is_bin_centroid)) {
    ## For bin centroids
    df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                                 counts_df = counts_df)

  } else {
    ## For bin means
    df_bin_centroids <- extract_hexbin_mean(data_hb = nldr_df_with_hex_id,
                                            counts_df = counts_df)

  }

  if (isFALSE(is_rm_lwd_hex)) {
    if (!missing(benchmark_to_rm_lwd_hex)) {
      stop("Need to initialise `is_rm_lwd_hex = TRUE`.")
    }

  }


  ## Do you need to remove low density hexagons?
  if (isTRUE(is_rm_lwd_hex)) {

    ## if the benchmark value to remove low density hexagons is not provided
    if (missing(benchmark_to_rm_lwd_hex)) {
      ## first quartile used as the default
      benchmark_to_rm_lwd_hex <- quantile(df_bin_centroids$std_counts,
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
      filter(std_counts <= benchmark_to_rm_lwd_hex)

    ## To identify low-density hexagons needed to remove by investigating neighbouring mean density
    identify_rm_bins <- find_low_dens_hex(df_bin_centroids_all = df_bin_centroids,
                                          bin1 = bin1,
                                          df_bin_centroids_low = df_bin_centroids_low)

    ## To remove low-density hexagons
    df_bin_centroids <- df_bin_centroids |>
      filter(!(hexID %in% identify_rm_bins))

  }

  ## To generate a data set with high-D and 2D training data
  df_all <- bind_cols(training_data |> select(-ID), nldr_df_with_hex_id)

  ## averaged high-D data
  df_bin <- avg_highd_data(data = df_all, col_start = col_start_highd)

  ## high-D model only contains the bins in 2D
  df_bin <- df_bin |>
    filter(hb_id %in% df_bin_centroids$hexID)

  return(list(df_bin = df_bin, df_bin_centroids = df_bin_centroids))

}

