#' Compute the Akaike Information Criterion (AIC) for a given model.
#'
#' @param p Number of dimensions of the data set.
#' @param total Total mean squared error (MSE) of the model.
#' @param num_bins Total number of bins used in the model.
#' @param num_obs Total number of observations in the training or test set.
#'
#' @return The AIC value for the specified model.
#' @export
#'
#' @examples
#' # Example usage of compute_aic function
#' p <- 5
#' total <- 1500
#' num_bins <- 10
#' num_obs <- 100
#' aic_value <- compute_aic(p, total, num_bins, num_obs)
#' cat("AIC Value:", aic_value, "\n")
#'
#'
#' @rdname compute_aic
compute_aic <- function(p, total, num_bins, num_obs) {
  mse <- mean(total) / p
  aic <- 2*num_bins*p + num_obs*p*log(mse)
  return(aic)
}


#' Predict Hexagonal IDs
#'
#' This function predicts hexagonal IDs for a test set based on training data and non-linear dimensionality reduction (NLDR) coordinates.
#'
#' @param training_data The training dataset containing high-dimensional data with IDs.
#' @param nldr_df The non-linear dimensionality reduction data frame containing 2D coordinates of the training data.
#' @param nldr_df_test The predicted non-linear dimensionality reductions for the test set.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#' @param shape_val The value of the shape parameter for hexagon binning.
#' @param x The name of the column that contains first embedding as a string.
#' @param y The name of the column that contains second embedding as a string.
#' @param col_start The text that begin the column name of the high-D data
#'
#' @return A list containing the predicted data, hexagonal bin centroids, averaged high-dimensional data, and hexbin_data_object.
#'
#' @importFrom dplyr bind_cols select mutate
#' @importFrom class knn
#'@importFrom rlang syms
#'
#' @examples
#' nldr_df <- s_curve_noise_umap
#' nldr_df_test <- s_curve_noise_umap_predict
#' training_data <- s_curve_noise_training
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_mean(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' predict_hex_id(training_data, nldr_df, nldr_df_test, num_bins,
#' shape_val, x = "UMAP1", y = "UMAP2", col_start = "x")
#'
#' @export
predict_hex_id <- function(training_data, nldr_df, nldr_df_test, num_bins, shape_val, x = "UMAP1", y = "UMAP2", col_start = "x") {

  ## To extract bin centroids
  hexbin_data_object <- extract_hexbin_centroids(nldr_df, num_bins, shape_val, x = x, y = y)

  df_bin_centroids <- hexbin_data_object$hexdf_data

  UMAP_data_with_hb_id <- nldr_df |>
    dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)

  ## To generate a data set with high-D and 2D training data
  df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), UMAP_data_with_hb_id)

  ## Averaged on high-D
  df_bin <- avg_highD_data(.data = df_all, column_start_text = col_start)

  train_hb_df <- df_bin_centroids |>
    dplyr::select(x, y, hexID)

  pred_hb_id <- class::knn(train_hb_df |> dplyr::select(-hexID), nldr_df_test |> dplyr::select(!!! rlang::syms(c(x, y))), cl = train_hb_df$hexID)

  pred_data <- nldr_df_test |>
    dplyr::mutate(pred_hb_id = as.numeric(as.character(pred_hb_id)))

  return(list(pred_data = pred_data, df_bin_centroids = df_bin_centroids, df_bin = df_bin, hexbin_data_object = hexbin_data_object))

}

#' Generate Evaluation Data Frame
#'
#' This function generates an evaluation data frame based on the provided data and predictions.
#'
#' @param data The data set containing high-dimensional data along with IDs.
#' @param prediction_df The data set with 2D embeddings, IDs, and predicted hexagonal IDs.
#' @param df_bin_centroids The data set with coordinates of hexagonal bin centroids.
#' @param df_bin The data set with averaged/weighted high-dimensional data.
#' @param num_bins Number of bins along the x-axis for hexagon binning.
#' @param col_start The text that begin the column name of the high-D data
#'
#' @return A tibble containing evaluation metrics based on the provided inputs.
#'
#' @importFrom dplyr select inner_join left_join mutate pick starts_with
#' @importFrom tibble tibble
#'
#' @examples
#' data <- s_curve_noise
#' nldr_df <- s_curve_noise_umap
#' nldr_df_test <- s_curve_noise_umap_predict
#' training_data <- s_curve_noise_training
#' num_bins <- 8
#' shape_val <- 2.031141
#' pred_df_test_object <- predict_hex_id(training_data = training_data,
#' nldr_df = nldr_df, nldr_df_test = nldr_df_test, num_bins = num_bins, shape_val = shape_val)
#' pred_df_test <- pred_df_test_object$pred_data
#' centroid_df_test <- pred_df_test_object$df_bin_centroids
#' avg_df_test <- pred_df_test_object$df_bin
#' generate_eval_df(data = data, prediction_df = pred_df_test,
#' df_bin_centroids = centroid_df_test, df_bin = avg_df_test, num_bins = num_bins, col_start = "x")
#'
#' @export
generate_eval_df <- function(data, prediction_df, df_bin_centroids, df_bin, num_bins, col_start = "x") {


  ## Generate all possible bin centroids in the full grid
  full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

  df_bin_centroids_filtered <- df_bin_centroids |>
    dplyr::select(hexID, x, y)

  ## To map centroid coordinates to predicted hexID
  prediction_df <- dplyr::inner_join(prediction_df, df_bin_centroids_filtered, by = c("pred_hb_id" = "hexID"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  prediction_df <- prediction_df |>
    dplyr::left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D averaged/weighted mean coordinates

  prediction_df <- prediction_df |>
    dplyr::left_join(data, by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    prediction_df[ , paste0("error_square_", col_start, i)] <- (prediction_df[ , paste0(col_start, i)] - prediction_df[ , paste0("avg_", col_start, i)])^2

  }

  prediction_df <- prediction_df |>
    dplyr::mutate(total = rowSums(dplyr::pick(tidyselect::starts_with(paste0("error_square_", col_start)))))


  #number_of_bins: Total number of bins with empty bins
  eval_df <- tibble::tibble(number_of_bins = NROW(full_centroid_df),
                            number_of_observations = NROW(prediction_df),
                            total_error = compute_aic((NCOL(df_bin) - 1), prediction_df$total, NROW(full_centroid_df), NROW(prediction_df)),
                            total_mse = mean(prediction_df$total)/(NCOL(df_bin) - 1))

  return(eval_df)

}

#' Predict 2D Embeddings
#'
#' Given a test dataset, the centroid coordinates of hexagonal bins in  2D and high-dimensional space,
#' predict the 2D embeddings for each data point in the test dataset.
#'
#' @param test_data The test dataset containing high-dimensional features and an ID column.
#' @param df_bin_centroids Centroid coordinates of hexagonal bins in 2D space.
#' @param df_bin Centroid coordinates of hexagonal bins in high dimensions.
#' @param type_NLDR The type of non-linear dimensionality reduction (NLDR) used. Default is "UMAP".
#'
#' @return A data frame with predicted 2D embedding for each data point in the test dataset.
#'
#' @examples
#' nldr_df <- s_curve_noise_umap
#' training_data <- s_curve_noise_training
#' test_data <- s_curve_noise_test
#' num_bins <- 8
#' shape_val <- 2.031141
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df, num_bins, shape_val)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' nldr_df_with_hb_id <- nldr_df |>
#' dplyr::mutate(hb_id = hexbin_data_object_s_curve$hb_data@cID)
#' df_all <- dplyr::bind_cols(training_data |> dplyr::select(-ID), nldr_df_with_hb_id)
#' df_bin <- avg_highD_data(.data = df_all, column_start_text = "x")
#' predict_2d_embeddings(test_data = test_data, df_bin_centroids = df_bin_centroids,
#' df_bin = df_bin, type_NLDR = "UMAP")
#'
#' @importFrom dplyr mutate_if bind_rows filter row_number arrange
#' @importFrom stats dist
#' @export
predict_2d_embeddings <- function(test_data, df_bin_centroids, df_bin, type_NLDR = "UMAP") {

  columns_df <- c(paste0("pred_", type_NLDR, "_", 1:2), "ID")
  vec <- stats::setNames(rep("", length(columns_df)), columns_df)  ## Define column names

  predict_coord_test <- dplyr::bind_rows(vec)[0, ]
  predict_coord_test <- predict_coord_test |>
    dplyr::mutate_if(is.character, as.numeric)

  for (i in 1:NROW(test_data)) {

    ### Filter the new data point
    test_data_point <- test_data |>
      dplyr::filter(dplyr::row_number() == i)

    ## Obtain centroid coordinates in high-D
    centroid_coord_high_D <- df_bin |>
      dplyr::select(-hb_id)

    ## Compute the distance between test point and the centroid points in high-D
    d <- stats::dist(dplyr::bind_rows(test_data_point |> dplyr::select(-ID), centroid_coord_high_D)) |> as.matrix()

    ## Obtain the distances
    distance_vec <- d[2:dim(d)[1], 1] |> as.vector()

    ## Add the distance vec as a column in high-D centroid coordinate data set
    centroid_coord_high_D <- centroid_coord_high_D |>
      dplyr::mutate(distance = distance_vec) |>
      dplyr::mutate(hb_id = df_bin$hb_id)

    ## Sort by distance and obtain the centroid which is nearest
    predict_centroid_coord_high_D <- centroid_coord_high_D |>
      dplyr::arrange(distance) |>
      dplyr::filter(dplyr::row_number() == 1)

    ## Rename columns
    #names(predict_centroid_coord_high_D)[1:(NCOL(test_data_point) - 1)] <- paste0("C_", names(predict_centroid_coord_high_D)[1:(NCOL(test_data_point) - 1)])

    ## Obtain 2D coordinate of the nearest high-D centroid
    predict_centroid_coord_2D <- df_bin_centroids |>
      dplyr::filter(hexID %in% predict_centroid_coord_high_D$hb_id) |>
      dplyr::select(x, y) |>
      dplyr::mutate(ID = test_data_point$ID)

    ## Rename columns
    names(predict_centroid_coord_2D) <- c(paste0("pred_", type_NLDR, "_", 1:2), "ID")

    ## Combine high-D and 2D coordinate
    #predict_centroid_coord_all <- dplyr::bind_cols(test_data_point, predict_centroid_coord_high_D, predict_centroid_coord_2D)

    ## Combine all
    predict_coord_test <- dplyr::bind_rows(predict_coord_test, predict_centroid_coord_2D)


  }

  return(predict_coord_test)


}


