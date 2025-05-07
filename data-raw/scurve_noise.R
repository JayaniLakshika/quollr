## This script is to generate the S-curve data in 7-D, the UMAP embedding with different (hyper)parameters, and highd-vis-model
library(umap)
library(rsample)
library(scales)
library(quollr)

set.seed(20230531)

#########################Generate data#########################################

## To generate S-curve data
s_curve <- function(n = 100) {
  tt <- 3 * pi * stats::runif(n = n_samples, min = -0.5, max = 0.5)
  x1 <- sin(tt)
  x2 <- 2.0 * stats::runif(n = n_samples)
  x3 <- sign(tt) * (cos(tt) - 1)

  tibble::tibble(
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

# Simulate some S-curve

sample_size <- 5000
scurve <- s_curve(n = sample_size) |>
  dplyr::mutate(x4 = stats::runif(sample_size, -0.02, 0.02),
                x5 = stats::runif(sample_size, -0.02, 0.02),
                x6 = stats::runif(sample_size, -0.1, 0.1),
                x7 = stats::runif(sample_size, -0.01, 0.01),
                ID = dplyr::row_number())

usethis::use_data(scurve, overwrite = TRUE)

#############################Generate UMAP #####################################

# Fit umap1
umap_config <- umap.defaults
umap_config$n_neighbors <- 15      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.1

UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)

scurve_umap <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap)[1:(ncol(scurve_umap))] <- paste0(rep("emb",(ncol(scurve_umap))), 1:(ncol(scurve_umap)))

scurve_umap <- scurve_umap |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap, overwrite = TRUE)

## predict umap embeddings
scurve_umap_predict <- predict(UMAP_fit, scurve |> dplyr::select(-ID)) |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap_predict)[1:(ncol(scurve_umap_predict))] <- paste0(rep("emb",(ncol(scurve_umap_predict))), 1:(ncol(scurve_umap_predict)))

scurve_umap_predict <- scurve_umap_predict |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap_predict, overwrite = TRUE)

## Fit umap2
umap_config <- umap.defaults
umap_config$n_neighbors <- 10      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.4

UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)

scurve_umap2 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap2)[1:(ncol(scurve_umap2))] <- paste0(rep("emb",(ncol(scurve_umap2))), 1:(ncol(scurve_umap2)))

scurve_umap2 <- scurve_umap2 |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap2, overwrite = TRUE)

## Fit umap3
umap_config <- umap.defaults
umap_config$n_neighbors <- 62      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.1

UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)

scurve_umap3 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap3)[1:(ncol(scurve_umap3))] <- paste0(rep("emb",(ncol(scurve_umap3))), 1:(ncol(scurve_umap3)))

scurve_umap3 <- scurve_umap3 |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap3, overwrite = TRUE)

## Fit umap4
umap_config <- umap.defaults
umap_config$n_neighbors <- 30      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.5

UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)

scurve_umap4 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap4)[1:(ncol(scurve_umap4))] <- paste0(rep("emb",(ncol(scurve_umap4))), 1:(ncol(scurve_umap4)))

scurve_umap4 <- scurve_umap4 |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap4, overwrite = TRUE)

## Fit umap5
umap_config <- umap.defaults
umap_config$n_neighbors <- 15      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.5

UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)

scurve_umap5 <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap5)[1:(ncol(scurve_umap5))] <- paste0(rep("emb",(ncol(scurve_umap5))), 1:(ncol(scurve_umap5)))

scurve_umap5 <- scurve_umap5 |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap5, overwrite = TRUE)

## Fit umap6
umap_config <- umap.defaults
umap_config$n_neighbors <- 46      # Set the number of neighbors
umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
umap_config$min_dist <- 0.9

UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)

scurve_umap <- UMAP_fit$layout |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap)[1:(ncol(scurve_umap))] <- paste0(rep("emb",(ncol(scurve_umap))), 1:(ncol(scurve_umap)))

scurve_umap <- scurve_umap |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap, overwrite = TRUE)

#####################################Fit the model #############################
scurve_model_obj <- list()

nldr_scaled_obj <- gen_scaled_data(
  data = scurve_umap)

scurve_umap_scaled <- nldr_scaled_obj$scaled_nldr

usethis::use_data(scurve_umap_scaled, overwrite = TRUE)

lim1 <- nldr_scaled_obj$lim1
lim2 <- nldr_scaled_obj$lim2
r2 <- diff(lim2)/diff(lim1)

scurve_model_obj[[1]] <- nldr_scaled_obj
names(scurve_model_obj)[1] <- "scurve_umap_scaled_obj"

hb_obj <- hex_binning(scurve_umap_scaled, bin1 = 15, r2 = r2, q = 0.1)

scurve_model_obj[[2]] <- hb_obj
names(scurve_model_obj)[2] <- "scurve_umap_hb_obj"

model_s_curve_obj <- fit_highd_model(
  highd_data = scurve,
  nldr_data = scurve_umap_scaled,
  bin1 = 15, r2 = r2)

scurve_model_obj[[3]] <- model_s_curve_obj
names(scurve_model_obj)[3] <- "scurve_umap_model_obj"

all_centroids_df <- hb_obj$centroids
counts_df <- hb_obj$std_cts

df_bin_centroids <- extract_hexbin_centroids(centroids_df = all_centroids_df,
                                             counts_df = counts_df)

tr1_object <- tri_bin_centroids(hex_df = df_bin_centroids, x = "c_x", y = "c_y")

scurve_model_obj[[4]] <- tr1_object
names(scurve_model_obj)[4] <- "scurve_umap_model_tr1_object"

tr_from_to_df <- gen_edges(tri_object = tr1_object, threshold = 0)

scurve_model_obj[[5]] <- tr_from_to_df
names(scurve_model_obj)[5] <- "scurve_umap_model_tr_from_to_df"

usethis::use_data(scurve_model_obj, overwrite = TRUE)
