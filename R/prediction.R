predict_hex_id <- function(data, nldr_df, num_bins) {

  data_split <- initial_split(nldr_df)
  training_data <- training(data_split) |>
    arrange(ID)
  test_data <- testing(data_split) |>
    arrange(ID)


  shape_val <- calculate_effective_shape_value(.data = training_data,
                                               x = UMAP1, y = UMAP2)

  hexbin <- create_hexbin_df(.data = data, nldr_df = training_data,
                             embedding_1 = UMAP1, embedding_2 = UMAP2,
                             num_bins = num_bins, shape_val = shape_val)


  df_all <- hexbin$df_new

  hb_object <- hexbin$hb

  df_bin <- avg_highD_data(.data = df_all)

  df_bin_centroids <- extract_hexbin_centroids(.data = df_bin, hb = hb_object)

  ## For training data

  train_hb_df <- df_bin_centroids |>
    dplyr::select(x_val_center, y_val_center, hb_id)

  pred_hb_id <- class::knn(train_hb_df |> select(-hb_id), training_data |> select(UMAP1, UMAP2), cl = train_hb_df$hb_id)

  pred_data_training <- training_data |>
    dplyr::mutate(pred_hb_id = as.numeric(pred_hb_id))

  df_bin_centroids_filtered <- df_bin_centroids |>
    dplyr::select(hb_id, x_val_center, y_val_center)

  pred_data_training <- dplyr::inner_join(pred_data_training, df_bin_centroids_filtered, by = c( "pred_hb_id" = "hb_id"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  pred_data_training <- pred_data_training |>
    dplyr::left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D weighted mean coordinates

  pred_data_training <- pred_data_training |>
    dplyr::left_join(data |> mutate(ID=row_number()), by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    pred_data_training[ , paste0("error_square_x", i)] <- (pred_data_training[ , paste0("x", i)] - pred_data_training[ , paste0("avg_x", i)])^2

  }

  pred_data_training <- pred_data_training |>
    dplyr::mutate(total = rowSums(pick(starts_with("error_square_x"))))

  pred_data_training <- pred_data_training |>
    dplyr::mutate(MSE_method_1 = 2 * num_bins * dim(data)[2] + NROW(training_data) * dim(data)[2] * log(total),
           MSE_method_2 = total * num_bins/NROW(training_data),
           number_of_bins = num_bins,
           number_of_observations = NROW(training_data))

  total_error_method_1 <- sum(pred_data_training$MSE_method_1)
  total_error_method_2 <- sum(pred_data_training$MSE_method_2)

  prediction_data_training <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(training_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)


  ## For test data

  train_hb_df <- df_bin_centroids |>
    dplyr::select(x_val_center, y_val_center, hb_id)

  pred_hb_id <- class::knn(train_hb_df |> select(-hb_id), test_data |> select(UMAP1, UMAP2), cl = train_hb_df$hb_id)

  pred_data <- test_data |>
    dplyr::mutate(pred_hb_id = as.numeric(pred_hb_id))

  df_bin_centroids_filtered <- df_bin_centroids |>
    dplyr::select(hb_id, x_val_center, y_val_center)

  pred_data <- dplyr::inner_join(pred_data, df_bin_centroids_filtered, by = c( "pred_hb_id" = "hb_id"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  pred_data <- pred_data |>
    dplyr::left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D weighted mean coordinates

  pred_data <- pred_data |>
    dplyr::left_join(data |> mutate(ID=row_number()), by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    pred_data[ , paste0("error_square_x", i)] <- (pred_data[ , paste0("x", i)] - pred_data[ , paste0("avg_x", i)])^2

  }

  pred_data <- pred_data |>
    dplyr::mutate(total = rowSums(pick(starts_with("error_square_x"))))

  pred_data <- pred_data |>
    dplyr::mutate(MSE_method_1 =  2 * num_bins * dim(data)[2] + NROW(test_data) * dim(data)[2] * log(total),
           MSE_method_2 = total * num_bins/NROW(test_data),
           number_of_bins = num_bins,
           number_of_observations = NROW(test_data))

  total_error_method_1 <- sum(pred_data$MSE_method_1)
  total_error_method_2 <- sum(pred_data$MSE_method_2)

  prediction_data_test <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(test_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)

  return(list(prediction_data_training = prediction_data_training, prediction_data_test = prediction_data_test))


}
