library(umap)
library(rsample)
library(scales)
library(quollr)

set.seed(20230531)

s_curve_obj <- list()

## To generate S-curve data
s_curve <- function(n_samples = 100) {
  tt <- 3 * pi * stats::runif(n = n_samples, min = -0.5, max = 0.5)
  x <- sin(tt)
  y <- 2.0 * stats::runif(n = n_samples)
  z <- sign(tt) * (cos(tt) - 1)
  X <- cbind(x, y, z)

  data.frame(X, stringsAsFactors = FALSE)
}

# Simulate some s_curve_noise

sample_size <- 5000
s_curve_noise <- s_curve(n_samples = sample_size)
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
UMAP_fit <- umap(s_curve_noise_training |> dplyr::select(-ID),
                 n_neighbors = 15, n_components =  2)

s_curve_noise_umap6 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap6)[1:(ncol(s_curve_noise_umap6))] <- paste0(rep("emb",(ncol(s_curve_noise_umap6))), 1:(ncol(s_curve_noise_umap6)))

s_curve_noise_umap6 <- s_curve_noise_umap6 |>
  dplyr::mutate(ID = s_curve_noise_training$ID)

usethis::use_data(s_curve_noise_umap6, overwrite = TRUE)

## predict umap embeddings
s_curve_noise_umap_predict <- predict(UMAP_fit, s_curve_noise_test |> dplyr::select(-ID)) |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap_predict)[1:(ncol(s_curve_noise_umap_predict))] <- paste0(rep("emb",(ncol(s_curve_noise_umap_predict))), 1:(ncol(s_curve_noise_umap_predict)))

s_curve_noise_umap_predict <- s_curve_noise_umap_predict |>
  dplyr::mutate(ID = s_curve_noise_test$ID)

usethis::use_data(s_curve_noise_umap_predict, overwrite = TRUE)

## Fit umap2
umap_config <- umap.defaults
umap_config$n_neighbors <- 10      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.4

UMAP_fit <- umap(s_curve_noise_training |> dplyr::select(-ID), config = umap_config)

s_curve_noise_umap2 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap2)[1:(ncol(s_curve_noise_umap2))] <- paste0(rep("emb",(ncol(s_curve_noise_umap2))), 1:(ncol(s_curve_noise_umap2)))

s_curve_noise_umap2 <- s_curve_noise_umap2 |>
  dplyr::mutate(ID = s_curve_noise_training$ID)

usethis::use_data(s_curve_noise_umap2, overwrite = TRUE)

## Fit umap3
umap_config <- umap.defaults
umap_config$n_neighbors <- 62      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.1

UMAP_fit <- umap(s_curve_noise_training |> dplyr::select(-ID), config = umap_config)

s_curve_noise_umap3 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap3)[1:(ncol(s_curve_noise_umap3))] <- paste0(rep("emb",(ncol(s_curve_noise_umap3))), 1:(ncol(s_curve_noise_umap3)))

s_curve_noise_umap3 <- s_curve_noise_umap3 |>
  dplyr::mutate(ID = s_curve_noise_training$ID)

usethis::use_data(s_curve_noise_umap3, overwrite = TRUE)

## Fit umap4
umap_config <- umap.defaults
umap_config$n_neighbors <- 30      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.5

UMAP_fit <- umap(s_curve_noise_training |> dplyr::select(-ID), config = umap_config)

s_curve_noise_umap4 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap4)[1:(ncol(s_curve_noise_umap4))] <- paste0(rep("emb",(ncol(s_curve_noise_umap4))), 1:(ncol(s_curve_noise_umap4)))

s_curve_noise_umap4 <- s_curve_noise_umap4 |>
  dplyr::mutate(ID = s_curve_noise_training$ID)

usethis::use_data(s_curve_noise_umap4, overwrite = TRUE)

## Fit umap5
umap_config <- umap.defaults
umap_config$n_neighbors <- 15      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.5

UMAP_fit <- umap(s_curve_noise_training |> dplyr::select(-ID), config = umap_config)

s_curve_noise_umap5 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap5)[1:(ncol(s_curve_noise_umap5))] <- paste0(rep("emb",(ncol(s_curve_noise_umap5))), 1:(ncol(s_curve_noise_umap5)))

s_curve_noise_umap5 <- s_curve_noise_umap5 |>
  dplyr::mutate(ID = s_curve_noise_training$ID)

usethis::use_data(s_curve_noise_umap5, overwrite = TRUE)

## Fit umap6
umap_config <- umap.defaults
umap_config$n_neighbors <- 46      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.9

UMAP_fit <- umap(s_curve_noise_training |> dplyr::select(-ID), config = umap_config)

s_curve_noise_umap <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(s_curve_noise_umap)[1:(ncol(s_curve_noise_umap))] <- paste0(rep("emb",(ncol(s_curve_noise_umap))), 1:(ncol(s_curve_noise_umap)))

s_curve_noise_umap <- s_curve_noise_umap |>
  dplyr::mutate(ID = s_curve_noise_training$ID)

usethis::use_data(s_curve_noise_umap, overwrite = TRUE)

nldr_scaled_obj <- gen_scaled_data(
  data = s_curve_noise_umap)

s_curve_noise_umap_scaled <- nldr_scaled_obj$scaled_nldr

usethis::use_data(s_curve_noise_umap_scaled, overwrite = TRUE)

lim1 <- nldr_scaled_obj$lim1
lim2 <- nldr_scaled_obj$lim2
r2 <- diff(lim2)/diff(lim1)

s_curve_obj[[1]] <- nldr_scaled_obj
names(s_curve_obj)[1] <- "s_curve_umap_scaled_obj"

hb_obj <- hex_binning(s_curve_noise_umap_scaled, bin1 = 15, r2 = r2, q = 0.1)

s_curve_obj[[2]] <- hb_obj
names(s_curve_obj)[2] <- "s_curve_umap_hb_obj"

model_s_curve_obj <- fit_highd_model(
  highd_data = s_curve_noise_training,
  nldr_data = s_curve_noise_umap_scaled,
  bin1 = 15, r2 = r2)

s_curve_obj[[3]] <- model_s_curve_obj
names(s_curve_obj)[3] <- "s_curve_umap_model_obj"

all_centroids_df <- hb_obj$centroids
counts_df <- hb_obj$std_cts

df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                             counts_df = counts_df)

tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")

s_curve_obj[[4]] <- tr1_object
names(s_curve_obj)[4] <- "s_curve_umap_model_tr1_object"

tr_from_to_df <- gen_edges(tri_object = tr1_object, threshold = 0)

s_curve_obj[[5]] <- tr_from_to_df
names(s_curve_obj)[5] <- "s_curve_umap_model_tr_from_to_df"

usethis::use_data(s_curve_obj, overwrite = TRUE)
