#' Construct the 2D model and lift into high-D
#'
#' This function fits a high-dimensional model using hexagonal bins and provides options
#' to customize the modeling process, including the choice of bin centroids or bin means,
#' removal of low-density hexagons, and averaging of high-dimensional data.
#'
#' @param training_data A data frame containing the training high-dimensional data.
#' @param nldr_df_with_id A data frame containing 2D embeddings with an unique identifier.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
#' @param cell_area The area of each hexagonal cell.
#' @param num_bins_x The number of bins along the x-axis for the hexagonal grid.
#' @param shape_val The shape parameter for the hexagons.
#' @param is_bin_centroid Logical, indicating whether to use bin centroids (default is TRUE).
#' @param is_rm_lwd_hex Logical, indicating whether to remove low-density hexagons (default is FALSE).
#' @param benchmark_to_rm_lwd_hex The benchmark value to remove low-density hexagons.
#' @param is_avg_high_d Logical, indicating whether to average the high-dimensional data within bins (default is TRUE).
#' @param column_start_text The text prefix for columns in the high-dimensional data.
#'
#' @return A list containing the data frame with high-dimensional coordinates for 2D bin centroids (\code{df_bin})
#' and the data frame containing information about hexagonal bin centroids (\code{df_bin_centroids}) in 2D.
#'
#' @examples
#' fit_high_d_model(training_data = s_curve_noise_training, nldr_df_with_id = s_curve_noise_umap)
#'
#' @export
fit_high_d_model <- function(training_data, nldr_df_with_id, x = "UMAP1",
                             y = "UMAP1", cell_area = 1, num_bins_x = NA, shape_val = NA,
                             is_bin_centroid = TRUE,
                             is_rm_lwd_hex = FALSE,
                             benchmark_to_rm_lwd_hex = NA,
                             is_avg_high_d = TRUE, column_start_text = "x") {

  ## If number of bins along the x-axis is not given
  if (is.na(num_bins_x)) {
    ## compute the number of bins along the x-axis
    num_bins_x <- calculate_effective_x_bins(.data = nldr_df_with_id, x = x,
                                             cell_area = cell_area)


  }

  ## If shape parameter is not given
  if (is.na(shape_val)) {
    ## compute shape parameter
    shape_val <- calculate_effective_shape_value(.data = nldr_df_with_id, x = x, y = y)

  }

  ## Do you need to use bin centroids or bin means?
  if (isTRUE(is_bin_centroid)) {
    ## For bin centroids
    hexbin_data_object <- extract_hexbin_centroids(nldr_df = nldr_df_with_id,
                                                   num_bins = num_bins_x,
                                                   shape_val = shape_val,
                                                   x = x, y = y)

  } else {
    ## For bin means
    hexbin_data_object <- extract_hexbin_mean(nldr_df = nldr_df_with_id,
                                                   num_bins = num_bins_x,
                                                   shape_val = shape_val,
                                              x = x, y = y)

  }



  ## Do you need to remove low density hexagons?
  if (isTRUE(is_rm_lwd_hex)) {
    ## extract bin centroid/bin mean info
    df_bin_centroids <- hexbin_data_object$hexdf_data

    ## if the benchmark value to remove low density hexagons is not provided
    if (is.na(benchmark_to_rm_lwd_hex)) {
      ## first quartile used as the default
      benchmark_to_rm_lwd_hex <- stats::quantile(df_bin_centroids$std_counts,
                                                 probs = c(0,0.25,0.5,0.75,1))[2]
      }

    ## To identify low density hexagons
    df_bin_centroids_low <- df_bin_centroids |>
      dplyr::filter(std_counts <= benchmark_to_rm_lwd_hex)

    ## To identify low-density hexagons needed to remove by investigating neighbouring mean density
    identify_rm_bins <- find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids,
                                                  num_bins_x = num_bins_x,
                                                  df_bin_centroids_low = df_bin_centroids_low)

    ## To remove low-density hexagons
    df_bin_centroids <- df_bin_centroids |>
      filter(!(hexID %in% identify_rm_bins))



  } else {
    ### Witout removing low density hexagons
    ## extract bin centroid/bin mean info
    df_bin_centroids <- hexbin_data_object$hexdf_data

    }


  nldr_df_with_hb_id <- nldr_df_with_id |>
    dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)

  ## To generate a data set with high-D and 2D training data
  df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), nldr_df_with_hb_id)

  ## Do you need to use bin centroids or bin means?
  if (isTRUE(is_avg_high_d)) {

    ## averaged high-D data
    df_bin <- avg_highD_data(.data = df_all, column_start_text = column_start_text)


  } else {

    ## weighted averaged high-D data
    df_bin <- weighted_highD_data(training_data = training_data,
                        nldr_df_with_id = nldr_df_with_id,
                        hb_object = hexbin_data_object, column_start_text = column_start_text)

  }

  ## high-D model only contains the bins in 2D
  df_bin <- df_bin |>
    dplyr::filter(hb_id %in% df_bin_centroids$hexID)

  return(list(df_bin = df_bin, df_bin_centroids = df_bin_centroids))

}
