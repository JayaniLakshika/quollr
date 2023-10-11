predict_method_1 <- function(nldr_df) {

  data_split <- initial_split(nldr_df)
  training_data <- training(data_split) |>
    arrange(ID)
  test_data <- testing(data_split) |>
    arrange(ID)

  num_bins <- num_bins_vec[i]

  shape_val <- calculate_effective_shape_value(.data = training_data,
                                               x = UMAP1, y = UMAP2)

  hexbin <- create_hexbin_df(.data = data, nldr_df = training_data,
                             embedding_1 = UMAP1, embedding_2 = UMAP2,
                             num_bins = num_bins, shape_val = shape_val)


  df_all <- hexbin$df_new

  hb_object <- hexbin$hb

  df_bin <- avg_highD_data(.data = df_all)

  df_bin_centroids <- extract_hexbin_centroids(.data = df_bin, hb = hb_object)

  tr1_object <- triangulate_bin_centroids(df_bin_centroids, x_val_center, y_val_center)

  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  distance <- cal_2D_dist(.data = tr_from_to_df)

  benchmark <- find_benchmark_value(.data = distance, distance_col = distance)

  ## For training data

  train_hb_df <- df_all |>
    select(UMAP1, UMAP2, hb_id)

  hb_training <- hexbin::hexbin(training_data |>
                                  dplyr::pull(UMAP1), training_data |>
                                  dplyr::pull(UMAP2), xbins = num_bins, IDs = TRUE, shape = shape_val)

  pred_data_training <- training_data |>
    mutate(pred_hb_id = hb_training@cID)

  df_bin_centroids_filtered <- df_bin_centroids |>
    select(hb_id, x_val_center, y_val_center)

  pred_data_training <- inner_join(pred_data_training, df_bin_centroids_filtered, by = c( "pred_hb_id" = "hb_id"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  pred_data_training <- pred_data_training |>
    left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D weighted mean coordinates

  pred_data_training <- pred_data_training |>
    left_join(data |> mutate(ID=row_number()), by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    pred_data_training[ , paste0("error_square_x", i)] <- (pred_data_training[ , paste0("x", i)] - pred_data_training[ , paste0("avg_x", i)])^2

  }

  pred_data_training <- pred_data_training |>
    mutate(total = rowSums(pick(starts_with("error_square_x"))))

  pred_data_training <- pred_data_training |>
    mutate(MSE_method_1 = total/NROW(training_data),
           MSE_method_2 = total/num_bins,
           number_of_bins = num_bins,
           number_of_observations = NROW(training_data))

  total_error_method_1 <- sum(pred_data_training$MSE_method_1)
  total_error_method_2 <- sum(pred_data_training$MSE_method_2)

  prediction_data_training <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(training_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)


  ## For test data

  train_hb_df <- df_all |>
    select(UMAP1, UMAP2, hb_id)

  #pred_hb_id <- knn(train_hb_df |> select(-hb_id), test_data |> select(UMAP1, UMAP2), cl = train_hb_df$hb_id)

  hb_test <- hexbin::hexbin(test_data |>
                              dplyr::pull(UMAP1), test_data |>
                              dplyr::pull(UMAP2), xbins = num_bins, IDs = TRUE, shape = shape_val)


  pred_data <- test_data |>
    mutate(pred_hb_id = hb_test@cID)

  df_bin_centroids_filtered <- df_bin_centroids |>
    select(hb_id, x_val_center, y_val_center)

  pred_data <- inner_join(pred_data, df_bin_centroids_filtered, by = c( "pred_hb_id" = "hb_id"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  pred_data <- pred_data |>
    left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D weighted mean coordinates

  pred_data <- pred_data |>
    left_join(data |> mutate(ID=row_number()), by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    pred_data[ , paste0("error_square_x", i)] <- (pred_data[ , paste0("x", i)] - pred_data[ , paste0("avg_x", i)])^2

  }

  pred_data <- pred_data |>
    mutate(total = rowSums(pick(starts_with("error_square_x"))))

  pred_data <- pred_data |>
    mutate(MSE_method_1 = total/NROW(test_data),
           MSE_method_2 = total/num_bins,
           number_of_bins = num_bins,
           number_of_observations = NROW(test_data))

  total_error_method_1 <- sum(pred_data$MSE_method_1)
  total_error_method_2 <- sum(pred_data$MSE_method_2)

  prediction_data_test <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(test_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)

  return(list(prediction_data_training = prediction_data_training, prediction_data_test = prediction_data_test))

}

predict_method_2 <- function(nldr_df) {

  data_split <- initial_split(UMAP_data)
  training_data <- training(data_split) |>
    arrange(ID)
  test_data <- testing(data_split) |>
    arrange(ID)

  num_bins <- num_bins_vec[i]

  # num_bins <- calculate_effective_x_bins(.data = training_data, x = UMAP1,
  #                                        cell_area = 1)

  shape_val <- calculate_effective_shape_value(.data = training_data,
                                               x = UMAP1, y = UMAP2)

  hexbin <- create_hexbin_df(.data = data, nldr_df = training_data,
                             embedding_1 = UMAP1, embedding_2 = UMAP2,
                             num_bins = num_bins, shape_val = shape_val)


  df_all <- hexbin$df_new

  hb_object <- hexbin$hb

  df_bin <- avg_highD_data(.data = df_all)

  df_bin_centroids <- extract_hexbin_centroids(.data = df_bin, hb = hb_object)

  tr1_object <- triangulate_bin_centroids(df_bin_centroids, x_val_center, y_val_center)

  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  distance <- cal_2D_dist(.data = tr_from_to_df)

  benchmark <- find_benchmark_value(.data = distance, distance_col = distance)

  hex_grid <- expand.grid(training_data |> pull(UMAP1), training_data |> pull(UMAP2))

  hex_grid_all <- expand.grid(min(training_data |> pull(UMAP1)): max(training_data |> pull(UMAP1)),
                              min(training_data |> pull(UMAP2)): max(training_data |> pull(UMAP2)))

  hex_grid <- dplyr::bind_rows(hex_grid, hex_grid_all) |>
    dplyr::distinct()


  hb_full <- hexbin::hexbin(x = hex_grid |> pull(Var1),
                            y = hex_grid |> pull(Var2),
                            xbins = num_bins, IDs = TRUE,
                            shape = shape_val)

  full_grid <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb_full)),  hexID = hb_full@cell)

  ## For training data

  train_hb_df <- df_all |>
    select(UMAP1, UMAP2, hb_id)

  ### Fit hexbins and store hexbin IDs
  hb_training <- hexbin::hexbin(training_data |>
                                  dplyr::pull(UMAP1), training_data |>
                                  dplyr::pull(UMAP2), xbins = num_bins, IDs = TRUE, shape = shape_val)

  ### Computes x and y coordinates from hexagon cell id's
  xy_training <- hexbin::hcell2xy(hb_training)

  df_cell_data_training <- tibble::tibble(ID = hb_training@cell, x_val_center_pred = xy_training$x, y_val_center_pred = xy_training$y)

  training_data <- training_data |>
    mutate(hb_id = hb_training@cID) |>
    left_join(df_cell_data_training, by = c("hb_id" = "ID"))


  pred_hb_id_list <- knn(full_grid |> select(-hexID), training_data |> select(x_val_center_pred, y_val_center_pred), cl = full_grid$hexID)

  pred_data_training <- training_data |>
    mutate(pred_hb_id = as.numeric(pred_hb_id_list))

  df_bin_centroids_filtered <- df_bin_centroids |>
    select(hb_id, x_val_center, y_val_center)

  pred_data_training <- inner_join(pred_data_training, df_bin_centroids_filtered, by = c( "pred_hb_id" = "hb_id"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  pred_data_training <- pred_data_training |>
    left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D weighted mean coordinates

  pred_data_training <- pred_data_training |>
    left_join(data |> mutate(ID=row_number()), by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    pred_data_training[ , paste0("error_square_x", i)] <- (pred_data_training[ , paste0("x", i)] - pred_data_training[ , paste0("avg_x", i)])^2

  }

  pred_data_training <- pred_data_training |>
    mutate(total = rowSums(pick(starts_with("error_square_x"))))

  pred_data_training <- pred_data_training |>
    mutate(MSE_method_1 = total/NROW(training_data),
           MSE_method_2 = total/num_bins,
           number_of_bins = num_bins,
           number_of_observations = NROW(training_data))

  total_error_method_1 <- sum(pred_data_training$MSE_method_1)
  total_error_method_2 <- sum(pred_data_training$MSE_method_2)

  prediction_data_training <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(training_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)


  ## For test data

  train_hb_df <- df_all |>
    select(UMAP1, UMAP2, hb_id)

  hex_grid <- expand.grid(test_data |> pull(UMAP1), test_data |> pull(UMAP2))

  hex_grid_all <- expand.grid(min(training_data |> pull(UMAP1)): max(training_data |> pull(UMAP1)),
                              min(training_data |> pull(UMAP2)): max(training_data |> pull(UMAP2)))

  hex_grid <- dplyr::bind_rows(hex_grid, hex_grid_all) |>
    dplyr::distinct()


  hb_full <- hexbin::hexbin(x = hex_grid |> pull(Var1),
                            y = hex_grid |> pull(Var2),
                            xbins = num_bins, IDs = TRUE,
                            shape = shape_val)

  full_grid <- tibble::tibble(tibble::as_tibble(hexbin::hcell2xy(hb_full)),  hexID = hb_full@cell)


  ### Fit hexbins and store hexbin IDs
  hb_test <- hexbin::hexbin(test_data |>
                              dplyr::pull(UMAP1), test_data |>
                              dplyr::pull(UMAP2), xbins = num_bins, IDs = TRUE, shape = shape_val)

  ### Computes x and y coordinates from hexagon cell id's
  xy <- hexbin::hcell2xy(hb_test)

  df_cell_data <- tibble::tibble(ID = hb_test@cell, x_val_center_pred = xy$x, y_val_center_pred = xy$y)

  test_data <- test_data |>
    mutate(hb_id = hb_test@cID) |>
    left_join(df_cell_data, by = c("hb_id" = "ID"))


  pred_hb_id_list <- knn(full_grid |> select(-hexID), test_data |> select(x_val_center_pred, y_val_center_pred), cl = full_grid$hexID)

  pred_data <- test_data |>
    mutate(pred_hb_id = as.numeric(pred_hb_id_list))

  df_bin_centroids_filtered <- df_bin_centroids |>
    select(hb_id, x_val_center, y_val_center)

  pred_data <- inner_join(pred_data, df_bin_centroids_filtered, by = c( "pred_hb_id" = "hb_id"))

  df_bin_train <- df_bin
  names(df_bin_train)[-1] <- paste0("avg_", names(df_bin_train)[-1])

  pred_data <- pred_data |>
    left_join(df_bin_train, by = c("pred_hb_id" = "hb_id")) ## Map high-D weighted mean coordinates

  pred_data <- pred_data |>
    left_join(data |> mutate(ID=row_number()), by = c("ID" = "ID")) ## Map high-D data

  for (i in 1:(NCOL(df_bin_train) - 1)) {

    pred_data[ , paste0("error_square_x", i)] <- (pred_data[ , paste0("x", i)] - pred_data[ , paste0("avg_x", i)])^2

  }

  pred_data <- pred_data |>
    mutate(total = rowSums(pick(starts_with("error_square_x"))))

  pred_data <- pred_data |>
    mutate(MSE_method_1 = total/NROW(test_data),
           MSE_method_2 = total/num_bins,
           number_of_bins = num_bins,
           number_of_observations = NROW(test_data))

  total_error_method_1 <- sum(pred_data$MSE_method_1)
  total_error_method_2 <- sum(pred_data$MSE_method_2)

  prediction_data_test <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(test_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)

  return(list(prediction_data_training = prediction_data_training, prediction_data_test = prediction_data_test))
}
