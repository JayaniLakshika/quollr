library(quollr)

library(dplyr)
library(snedata)
library(langevitour)
library(ggplot2)
library(purrr) ## map function
library(gridExtra) ## for grid.arrange
library(rsample)
library(DT)
library(ggbeeswarm)
library(umap)
library(class)


set.seed(20230531)

# Simulate some data

sample_size <- 1000
data <- snedata::s_curve(n_samples = sample_size)
data <- data %>%
  select(-color)
names(data) <- c("x1", "x2", "x3")

data$x4 <- runif(sample_size, -0.02, 0.02)
data$x5 <- runif(sample_size, -0.02, 0.02)
data$x6 <- runif(sample_size, -0.1, 0.1)
data$x7 <- runif(sample_size, -0.01, 0.01)

langevitour(data)

fit_umap <- function(data, n_neighbors = NA, num_dim = 2, with_seed = NULL, ...){
  # If a seed is specified, then use it, otherwise ignore
  if(!is.null(with_seed)){set.seed(with_seed)}

  if(is.na(n_neighbors)){
    n_neighbors <- 15
  }

  UMAP_fit <- data %>%
    select(where(is.numeric)) %>%
    umap(n_neighbors = n_neighbors, n_components =  num_dim)

  UMAP_df <- UMAP_fit$layout %>%
    as.data.frame()  %>%
    mutate(ID=row_number())

  names(UMAP_df)[1:(ncol(UMAP_df)-1)] <- paste0(rep("UMAP",(ncol(UMAP_df)-1)), 1:(ncol(UMAP_df)-1))
  return(list(UMAP_fit = UMAP_fit, UMAP_df = UMAP_df))
}

plot_umap_2d <- function(UMAP_df){
  UMAP_df_plot <- UMAP_df %>%
    ggplot(aes(x = UMAP1,
               y = UMAP2))+
    geom_point(alpha=0.5) +
    coord_equal() +
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.text = element_text(size = 5),
          axis.title = element_text(size = 7))
  return(UMAP_df_plot)
}

UMAP_data <- fit_umap(data, n_neighbors = 50, with_seed = 20230531)$UMAP_df

data_split <- initial_split(UMAP_data)
training_data <- training(data_split) |>
  dplyr::arrange(ID)
test_data <- testing(data_split) |>
  dplyr::arrange(ID)

plot_list2 <- plot_umap_2d(training_data) +
  theme_linedraw() +
  theme(plot.title = element_text(size = 7, hjust = 0.5, vjust = -0.5),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_list2


num_bins_x <- calculate_effective_x_bins(.data = training_data, x = UMAP1,
                                         cell_area = 1)
num_bins_x

shape_val <- calculate_effective_shape_value(.data = training_data,
                                             x = UMAP1, y = UMAP2)
shape_val

hexgrid_with_points <- draw_full_hexgrid(.data = data, nldr_df = training_data,
                                         embedding_1 = UMAP1, embedding_2 = UMAP2,
                                         num_bins = num_bins_x, shape_val = shape_val)
hexgrid_with_points

df_with_hexID <- create_hexbin_df(.data = data, nldr_df = training_data,
                               embedding_1 = UMAP1, embedding_2 = UMAP2,
                               num_bins = num_bins_x, shape_val = shape_val)

df_all <- df_with_hexID$df_new

hb_object <- df_with_hexID$hb

df_all |>
  datatable()

df_bin <- avg_highD_data(.data = df_all)

df_bin |>
  datatable()

df_bin_centroids <- extract_hexbin_centroids(.data = df_bin, hb = hb_object)

df_bin_centroids |>
  datatable()

tr1_object <- triangulate_bin_centroids(df_bin_centroids, x_val_center, y_val_center)

tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

distance <- cal_2D_dist(.data = tr_from_to_df)

distance_plot <- plot_dist(distance) +
  ylab(expression(d^{(2)})) +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 12))

distance_plot

benchmark <- find_benchmark_value(.data = distance, distance_col = distance)
benchmark

trimesh_gr <- colour_long_edges(.data = distance, benchmark_value = benchmark,
                                triangular_object = tr1_object, distance_col = distance)
trimesh_gr

trimesh_removed <- remove_long_edges(.data = distance, benchmark_value = benchmark,
                                     triangular_object = tr1_object, distance_col = distance)
trimesh_removed

tour1 <- show_langevitour(df_all, df_bin, df_bin_centroids, benchmark_value = benchmark, distance = distance, distance_col = distance)
tour1

## Prediction

train_hb_df <- df_all |>
  select(UMAP1, UMAP2, hb_id)

pred_hb_id <- knn(train_hb_df |> select(-hb_id), test_data |> select(UMAP1, UMAP2), cl = train_hb_df$hb_id)

pred_data <- test_data |>
  mutate(pred_hb_id = as.numeric(pred_hb_id))

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

pred_data |>
  DT::datatable()

### Method 1
pred_data <- pred_data |>
  mutate(MSE_method_1 = total/NROW(test_data))

total_error_method_1 <- sum(pred_data$MSE_method_1)
total_error_method_1

### Method 2
pred_data <- pred_data |>
  mutate(MSE_method_2 = total/num_bins_x)

total_error_method_2 <- sum(pred_data$MSE_method_2)
total_error_method_2


num_bins_vec <- append(sample(2:90, 40), num_bins_x)

vec <- stats::setNames(rep("", 4), c("number_of_bins", "number_of_observations", "total_error_method_1", "total_error_method_2"))  ## Define column names
prediction_data <- dplyr::bind_rows(vec)[0, ]

prediction_data <- prediction_data |>
  dplyr::mutate_if(is.character, as.numeric)

prediction_data_training <- dplyr::bind_rows(vec)[0, ]

prediction_data_training <- prediction_data_training |>
  dplyr::mutate_if(is.character, as.numeric)

for (i in 1:length(num_bins_vec)) {

  data_split <- initial_split(UMAP_data)
  training_data <- training(data_split) |>
    arrange(ID)
  test_data <- testing(data_split) |>
    arrange(ID)

  num_bins <- num_bins_vec[i]

  shape_val <- calculate_effective_shape_value(.data = training_data,
                                               x = UMAP1, y = UMAP2)

  hexgrid_with_points <- create_hexbin_df(.data = data, nldr_df = training_data,
                          embedding_1 = UMAP1, embedding_2 = UMAP2,
                          num_bins = num_bins, shape_val = shape_val)


  df_all <- hexgrid_with_points$df_new

  hb_object <- hexgrid_with_points$hb

  df_bin <- avg_highD_data(.data = df_all)

  df_bin_centroids <- extract_hexbin_centroids(.data = df_bin, hb = hb_object)

  tr1_object <- triangulate_bin_centroids(df_bin_centroids, x_val_center, y_val_center)

  tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

  distance <- cal_2D_dist(.data = tr_from_to_df)

  benchmark <- find_benchmark_value(.data = distance, distance_col = distance)

  train_hb_df <- df_all |>
    select(UMAP1, UMAP2, hb_id)

  pred_hb_id <- knn(train_hb_df |> select(-hb_id), training_data |> select(UMAP1, UMAP2), cl = train_hb_df$hb_id)

  pred_data_training <- training_data |>
    mutate(pred_hb_id = as.numeric(pred_hb_id))

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

  summary_df_training <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(training_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)

  prediction_data_training <- bind_rows(prediction_data_training, summary_df_training)


  ## For test data

  train_hb_df <- df_all |>
    select(UMAP1, UMAP2, hb_id)

  pred_hb_id <- knn(train_hb_df |> select(-hb_id), test_data |> select(UMAP1, UMAP2), cl = train_hb_df$hb_id)

  pred_data <- test_data |>
    mutate(pred_hb_id = as.numeric(pred_hb_id))

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

  summary_df <- tibble::tibble(number_of_bins = num_bins, number_of_observations = NROW(test_data), total_error_method_1 = total_error_method_1, total_error_method_2 = total_error_method_2)

  prediction_data <- bind_rows(prediction_data, summary_df)


}

prediction_data |>
  datatable()

prediction_data <- prediction_data |>
  mutate(data_type = "test")

prediction_data_training <- prediction_data_training |>
  mutate(data_type = "training")

MSE_df <- bind_rows(prediction_data, prediction_data_training)

ggplot(MSE_df, aes(x = number_of_bins,
                   y = total_error_method_1,
                   color = data_type
)) +
  geom_line() +
  geom_point() +
  # scale_fill_discrete(name = "Method", labels = c("Using the number of observations in test data", "Using the number of bins")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  ylab("Total MSE from method 1")

ggplot(MSE_df, aes(x = number_of_bins,
                   y = total_error_method_2,
                   color = data_type
)) +
  geom_line() +
  geom_point() +
  # scale_fill_discrete(name = "Method", labels = c("Using the number of observations in test data", "Using the number of bins")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  ylab("Total MSE from method 2")


