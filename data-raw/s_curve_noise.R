library(snedata)
library(umap)
library(rsample)

set.seed(20230531)

# Simulate some s_curve_noise

sample_size <- 100
s_curve_noise <- snedata::s_curve(n_samples = sample_size)
s_curve_noise <- s_curve_noise |>
  dplyr::select(-color)
names(s_curve_noise) <- c("x1", "x2", "x3")

s_curve_noise$x4 <- runif(sample_size, -0.02, 0.02)
s_curve_noise$x5 <- runif(sample_size, -0.02, 0.02)
s_curve_noise$x6 <- runif(sample_size, -0.1, 0.1)
s_curve_noise$x7 <- runif(sample_size, -0.01, 0.01)

## Add the ID to the s_curve_noise
s_curve_noise <- s_curve_noise |>
  dplyr::mutate(ID = dplyr::row_number()) |>
  tibble::as_tibble()

usethis::use_data(s_curve_noise, overwrite = TRUE)

## Split the s_curve_noise as training and test
data_split <- initial_split(s_curve_noise)
s_curve_noise_training <- training(data_split) |>
  dplyr::arrange(ID) |>
  tibble::as_tibble()

usethis::use_data(s_curve_noise_training, overwrite = TRUE)

s_curve_noise_test <- testing(data_split) |>
  dplyr::arrange(ID) |>
  tibble::as_tibble()

usethis::use_data(s_curve_noise_test, overwrite = TRUE)


## Fit umap
UMAP_fit <- umap(s_curve_noise_training |> dplyr::select(-ID), n_neighbors = 15, n_components =  2)

s_curve_noise_umap <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap)[1:(ncol(s_curve_noise_umap))] <- paste0(rep("UMAP",(ncol(s_curve_noise_umap))), 1:(ncol(s_curve_noise_umap)))

s_curve_noise_umap <- s_curve_noise_umap |>
  dplyr::mutate(ID = s_curve_noise_training$ID)

usethis::use_data(s_curve_noise_umap, overwrite = TRUE)


## predict umap embeddings
s_curve_noise_umap_predict <- predict(UMAP_fit, s_curve_noise_test |> dplyr::select(-ID)) |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap_predict)[1:(ncol(s_curve_noise_umap_predict))] <- paste0(rep("UMAP",(ncol(s_curve_noise_umap_predict))), 1:(ncol(s_curve_noise_umap_predict)))

s_curve_noise_umap_predict <- s_curve_noise_umap_predict |>
  dplyr::mutate(ID = s_curve_noise_test$ID)

usethis::use_data(s_curve_noise_umap_predict, overwrite = TRUE)

