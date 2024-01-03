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

library(plotly)


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

## Add the ID to the data

data <- data |>
  mutate(ID = row_number())

## Split the data as training and test
data_split <- initial_split(data)
training_data <- training(data_split) |>
  arrange(ID)
test_data <- testing(data_split) |>
  arrange(ID)

## Fit umap
plot_umap_2d <- function(UMAP_df){
  UMAP_df_plot <- UMAP_df %>%
    ggplot(aes(x = UMAP1,
               y = UMAP2,
               label = ID))+
    geom_point(alpha=0.5) +
    coord_equal() +
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.text = element_text(size = 5),
          axis.title = element_text(size = 7))
  return(UMAP_df_plot)
}

UMAP_fit <- umap(training_data |> dplyr::select(-ID), n_neighbors = 15, n_components =  2)

UMAP_data <- UMAP_fit$layout |>
  as.data.frame()
names(UMAP_data)[1:(ncol(UMAP_data))] <- paste0(rep("UMAP",(ncol(UMAP_data))), 1:(ncol(UMAP_data)))

UMAP_data <- UMAP_data |>
  mutate(ID = training_data$ID)

plot_umap_2d( UMAP_data)
ggplotly()

## predict umap embeddings

predict_UMAP_df <- predict(UMAP_fit, test_data |> dplyr::select(-ID)) |>
  as.data.frame()

names(predict_UMAP_df)[1:(ncol(predict_UMAP_df))] <- paste0(rep("UMAP",(ncol(predict_UMAP_df))), 1:(ncol(predict_UMAP_df)))

predict_UMAP_df <- predict_UMAP_df |>
  mutate(ID = test_data$ID)

plot_umap_2d(UMAP_data) +
  geom_point(data = predict_UMAP_df, aes(x = UMAP1, y = UMAP2), color = "red")
ggplotly()

## Calculate number of bins along x-axis
num_bins_x <- calculate_effective_x_bins(.data = UMAP_data, x = UMAP1,
                                         cell_area = 1)
num_bins_x

## Calculate shape parameter

shape_val <- calculate_effective_shape_value(.data = UMAP_data,
                                             x = UMAP1, y = UMAP2)
shape_val

## To extract bin centroids

hexbin_data_object <- extract_hexbin_centroids(UMAP_data, num_bins_x, shape_val)

df_bin_centroids <- hexbin_data_object$hexdf_data

df_bin_centroids |>
  head() |>
  DT::datatable()

# ggplot(df_bin_centroids, aes(x = x, y = y, label = hexID)) +
#   geom_text() +
#   coord_equal()

## Data set with all possible centroids in the hexagonal grid

full_centroid_df <- generate_full_grid_centroids(df_bin_centroids)

full_centroid_df |>
  head(5) |>
  DT::datatable()

## Generate all coordinates of hexagons
hex_grid <- full_hex_grid(full_centroid_df)

ggplot(data = hex_grid, aes(x = x, y = y)) + geom_polygon(fill = "white", color = "black", aes(group = id)) +
  geom_point(data = hex_grid, aes(x = x, y = y), color = "black") +
  geom_point(data = df_bin_centroids, aes(x = x, y = y), color = "red")

ggplot(data = hex_grid, aes(x = x, y = y)) + geom_polygon(fill = "white", color = "black", aes(group = id)) +
  geom_point(data = full_centroid_df, aes(x = x, y = y), color = "black") +
  geom_point(data = df_bin_centroids, aes(x = x, y = y), color = "red")

hex_full_count_df <- generate_full_grid_info(df_bin_centroids)

ggplot(data = hex_full_count_df, aes(x = x, y = y)) +
  geom_polygon(color = "black", aes(group = polygon_id, fill = std_counts)) +
  geom_text(aes(x = c_x, y = c_y, label = hexID)) +
  scale_fill_viridis_c(direction = -1, na.value = "#ffffff")

## To identify low-density hexagons

## Check
# ggplot(df_bin_centroids_coordinates, aes(x = reorder(as.factor(hexID), -mean_density), y = mean_density)) + geom_point()
#
# df_bin_centroids_coordinates$group <- "1"
#
# ggplot(df_bin_centroids_coordinates, aes(x = group, y = mean_density)) +
#   geom_quasirandom()

## Identify bins


identify_rm_bins <- find_low_density_hexagons(df_bin_centroids)

df_bin_centroids <- df_bin_centroids |>
  filter(!(hexID %in% identify_rm_bins))





## Add hexbin Id to 2D embeddings
UMAP_data_with_hb_id <- UMAP_data |>
  mutate(hb_id = hexbin_data_object$hb_data@cID)

## To generate data set with point info
full_grid_with_hexbin_id <- map_hexbin_id(full_centroid_df, df_bin_centroids)

pts_df <- find_pts_in_hexbins(full_grid_with_hexbin_id, UMAP_data_with_hb_id)

pts_df <- dplyr::full_join(full_grid_with_hexbin_id, pts_df, by = c("hexID" = "hexID")) |>
  distinct()

pts_df |>
  head() |>
  datatable()


## To generate a data set with high-D and 2D training data
df_all <- training_data |> dplyr::select(-ID) |>
  dplyr::bind_cols(UMAP_data_with_hb_id)

df_all |>
  head() |>
  DT::datatable()

## To generate averaged high-D data

df_bin <- avg_highD_data(.data = df_all) ## Need to pass ID column name

df_bin |>
  head() |>
  DT::datatable()

## Triangulate bin centroids

tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)

tr_from_to_df |>
  head() |>
  DT::datatable()

## Compute 2D distances
distance <- cal_2D_dist(.data = tr_from_to_df)

## To plot the distribution of distance
plot_dist <- function(distance_df){
  distance_df$group <- "1"
  dist_plot <- ggplot(distance_df, aes(x = group, y = distance)) +
    geom_quasirandom()+
    ylim(0, max(unlist(distance_df$distance))+ 0.5) + coord_flip()
  return(dist_plot)
}

distance_plot <- plot_dist(distance) +
  #ggtitle("(b)" ) +
  ylab(expression(d^{(2)})) +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 12))

distance_plot


## To draw the traingular mesh

trimesh <- ggplot(df_bin_centroids, aes(x = x, y = y)) +
  geom_point(size = 0.1) +
  geom_trimesh() +
  coord_equal()

trimesh <- trimesh +
  #ggtitle("(a)") +
  xlab(expression(C[x]^{(2)})) + ylab(expression(C[y]^{(2)})) +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 7))

trimesh

## To find the benchmark value to remove long edges

benchmark <- find_benchmark_value(.data = distance, distance_col = distance)
benchmark

## To draw the colored long edges in 2D

trimesh_gr <- colour_long_edges(.data = distance, benchmark_value = benchmark,
                                triangular_object = tr1_object, distance_col = distance)

trimesh_gr <- trimesh_gr +
  xlab(expression(C[x]^{(2)})) + ylab(expression(C[y]^{(2)})) +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 7))

trimesh_gr

## To draw after removing long edges in 2D

trimesh_removed <- remove_long_edges(.data = distance, benchmark_value = benchmark,
                                     triangular_object = tr1_object, distance_col = distance)

trimesh_removed <- trimesh_removed +
  xlab(expression(C[x]^{(2)})) + ylab(expression(C[y]^{(2)})) +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 7))

trimesh_removed

tour1 <- show_langevitour(df_all, df_bin, df_bin_centroids, benchmark_value = benchmark, distance = distance, distance_col = distance)
tour1


## Prediction

shape_value <- calculate_effective_shape_value(.data = UMAP_data,
                                               x = UMAP1, y = UMAP2)

num_bins_vec <- 1:10 ## Number of bins along the x-axis

vec <- stats::setNames(rep("", 6), c("number_of_bins", "number_of_observations", "total_error", "totol_error_method_2", "totol_error_method_3", "total_mse"))  ## Define column names

eval_data_test <- dplyr::bind_rows(vec)[0, ]
eval_data_test <- eval_data_test |>
  dplyr::mutate_if(is.character, as.numeric)

eval_data_training <- dplyr::bind_rows(vec)[0, ]
eval_data_training <- eval_data_training |>
  dplyr::mutate_if(is.character, as.numeric)

for (i in 1:length(num_bins_vec)) {

  pred_df_training_object <- predict_hex_id(training_data = training_data, nldr_df = UMAP_data, nldr_df_test = UMAP_data, num_bins = num_bins_vec[i], shape_val = shape_value)
  pred_df_training <- pred_df_training_object$pred_data
  centroid_df_training <- pred_df_training_object$df_bin_centroids
  avg_df_training <- pred_df_training_object$df_bin

  eval_df_training <- generate_eval_df(data = data, prediction_df = pred_df_training, df_bin_centroids = centroid_df_training, df_bin = avg_df_training, num_bins = num_bins_vec[i])

  pred_df_test_object <- predict_hex_id(training_data = training_data, nldr_df = UMAP_data, nldr_df_test = predict_UMAP_df, num_bins = num_bins_vec[i], shape_val = shape_value)
  pred_df_test <- pred_df_test_object$pred_data
  centroid_df_test <- pred_df_test_object$df_bin_centroids
  avg_df_test <- pred_df_test_object$df_bin

  eval_df_test <- generate_eval_df(data = data, prediction_df = pred_df_test, df_bin_centroids = centroid_df_test, df_bin = avg_df_test, num_bins = num_bins_vec[i])

  eval_data_training <- dplyr::bind_rows(eval_data_training, eval_df_training)
  eval_data_test <- dplyr::bind_rows(eval_data_test, eval_df_test)

}


## Add new column with data types

eval_data_training <- eval_data_training |>
  mutate(data_type = "training")

eval_data_test <- eval_data_test |>
  mutate(data_type = "test")

MSE_df <- dplyr::bind_rows(eval_data_training, eval_data_test)

## To draw with AIC
ggplot(MSE_df |> dplyr::filter(data_type == "training"), aes(x = number_of_bins,
                                                             y = total_error,
                                                             color = data_type
)) +
  geom_point() +
  geom_line() +
  #geom_vline(xintercept = NROW(full_grid_with_hexbin_id)) +
  #annotate("text", x= (NROW(full_grid_with_hexbin_id) - 10), y=-5000, label=paste0("effective number of bins = ", as.character(NROW(full_grid_with_hexbin_id))), angle=90) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  ylab("AIC") +
  xlab("Total number of bins")
## Effective number of bins along x-axis

ggplot(MSE_df, aes(x = number_of_bins,
                   y = total_mse,
                   color = data_type
)) +
  geom_point() +
  geom_line() +
  # geom_vline(xintercept = NROW(full_grid_with_hexbin_id)) +
  # annotate("text", x= (NROW(full_grid_with_hexbin_id) - 10), y=0.25, label=paste0("effective number of bins = ", as.character(NROW(full_grid_with_hexbin_id))), angle=90) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  ylab("MSE") +
  xlab("Total number of bins")

