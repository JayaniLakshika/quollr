## This script is to generate the S-curve data in 7-D, the UMAP embedding with different (hyper)parameters, and highd-vis-model
library(umap)
library(quollr)
library(ggplot2)
library(dplyr)

set.seed(20230531)

#########################Generate data#########################################

## To generate S-curve data
s_curve <- function(n = 100) {
  tt <- 3 * pi * stats::runif(n = n, min = -0.5, max = 0.5)
  x1 <- sin(tt)
  x2 <- 2.0 * stats::runif(n = n)
  x3 <- sign(tt) * (cos(tt) - 1)

  tibble::tibble(
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

# Simulate some S-curve

sample_size <- 1000
scurve <- s_curve(n = sample_size) |>
  dplyr::mutate(x4 = stats::runif(sample_size, -0.02, 0.02),
                x5 = stats::runif(sample_size, -0.02, 0.02),
                x6 = stats::runif(sample_size, -0.1, 0.1),
                x7 = stats::runif(sample_size, -0.01, 0.01),
                ID = dplyr::row_number())

usethis::use_data(scurve, overwrite = TRUE)

#############################Generate UMAP #####################################

# # Fit umap1
# umap_config <- umap.defaults
# umap_config$n_neighbors <- 15      # Set the number of neighbors
# umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
# umap_config$min_dist <- 0.1
#
# UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)
#
# scurve_umap6 <- UMAP_fit$layout |>
#   as.data.frame() |>
#   tibble::as_tibble()
#
# names(scurve_umap6)[1:(ncol(scurve_umap6))] <- paste0(rep("emb",(ncol(scurve_umap6))), 1:(ncol(scurve_umap6)))
#
# scurve_umap6 <- scurve_umap6 |>
#   dplyr::mutate(ID = scurve$ID)
#
# usethis::use_data(scurve_umap6, overwrite = TRUE)

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

# ## Fit umap5
# umap_config <- umap.defaults
# umap_config$n_neighbors <- 15      # Set the number of neighbors
# umap_config$n_components <- 2    # Set the number of output dimensions (typically 2 or 3)
# umap_config$min_dist <- 0.5
#
# UMAP_fit <- umap(scurve |> dplyr::select(-ID), config = umap_config)
#
# scurve_umap5 <- UMAP_fit$layout |>
#   as.data.frame() |>
#   tibble::as_tibble()
#
# names(scurve_umap5)[1:(ncol(scurve_umap5))] <- paste0(rep("emb",(ncol(scurve_umap5))), 1:(ncol(scurve_umap5)))
#
# scurve_umap5 <- scurve_umap5 |>
#   dplyr::mutate(ID = scurve$ID)
#
# usethis::use_data(scurve_umap5, overwrite = TRUE)
#
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

## predict umap embeddings
scurve_umap_predict <- predict(UMAP_fit, scurve |> dplyr::select(-ID)) |>
  as.data.frame() |>
  tibble::as_tibble()

names(scurve_umap_predict)[1:(ncol(scurve_umap_predict))] <- paste0(rep("emb",(ncol(scurve_umap_predict))), 1:(ncol(scurve_umap_predict)))

scurve_umap_predict <- scurve_umap_predict |>
  dplyr::mutate(ID = scurve$ID)

usethis::use_data(scurve_umap_predict, overwrite = TRUE)

#####################################Fit the model #############################

scurve_model_obj <- fit_highd_model(
  highd_data = scurve,
  nldr_data = scurve_umap,
  b1 = 15,
  benchmark_highd = 10)

usethis::use_data(scurve_model_obj, overwrite = TRUE)

########################Generate errors for different bin widths################
scurve_umap_rmse <- gen_diffbin1_errors(highd_data = scurve, nldr_data = scurve_umap) |>
  dplyr::mutate(method = "UMAP")
usethis::use_data(scurve_umap_rmse, overwrite = TRUE)

scurve_umap_rmse2 <- gen_diffbin1_errors(highd_data = scurve, nldr_data = scurve_umap2) |>
  dplyr::mutate(method = "UMAP2")
usethis::use_data(scurve_umap_rmse2, overwrite = TRUE)

scurve_umap_rmse3 <- gen_diffbin1_errors(highd_data = scurve, nldr_data = scurve_umap3) |>
  dplyr::mutate(method = "UMAP3")
usethis::use_data(scurve_umap_rmse3, overwrite = TRUE)

scurve_umap_rmse4 <- gen_diffbin1_errors(highd_data = scurve, nldr_data = scurve_umap4) |>
  dplyr::mutate(method = "UMAP4")
usethis::use_data(scurve_umap_rmse4, overwrite = TRUE)


#####################################Plot RMSE #############################

error_df <- bind_rows(scurve_umap_rmse, scurve_umap_rmse2,
                      scurve_umap_rmse3, scurve_umap_rmse4) |>
  mutate(a1 = round(a1, 2)) |>
  filter(b1 >= 5) |>
  group_by(method, a1) |>
  filter(RMSE == min(RMSE)) |>
  ungroup()

rmse_plot <- ggplot(error_df,
       aes(x = a1,
           y = RMSE,
           colour = method)) +
  geom_point(size = 0.8) +
  geom_line(linewidth = 0.3) +
  scale_x_continuous(breaks =
                       sort(unique(error_df$a1))[
                         seq(1, length(
                           unique(error_df$a1)),
                           by = 5)]) +
  scale_color_manual(
    values=c('#e41a1c','#ff7f00','#4daf4a',
             "#a65628",'#636363', '#984ea3')) +
  theme(aspect.ratio = 1.5) +
  ylab("RMSE") +
  xlab(expression(paste("binwidth (", a[1], ")"))) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = 'transparent'),
        plot.title = element_text(size = 12, hjust = 0.5, vjust = -0.5),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        legend.position = "none",
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        plot.margin = margin(0, 0, 0, 0))

#####################################Plot NLDR layouts #############################

nldr1 <- scurve_umap |>
  ggplot(aes(x = emb1,
             y = emb2)) +
  geom_point(alpha=0.1, size=1, colour="#e41a1c") +
  theme_linedraw() +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

nldr2 <- scurve_umap2 |>
  ggplot(aes(x = emb1,
             y = emb2)) +
  geom_point(alpha=0.1, size=1, colour="#ff7f00") +
  theme_linedraw() +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

nldr3 <- scurve_umap3 |>
  ggplot(aes(x = emb1,
             y = emb2)) +
  geom_point(alpha=0.1, size=1, colour="#4daf4a") +
  theme_linedraw() +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

nldr4 <- scurve_umap4 |>
  ggplot(aes(x = emb1,
             y = emb2)) +
  geom_point(alpha=0.1, size=1, colour="#a65628") +
  theme_linedraw() +
  theme(plot.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

scurve_plts <- list(rmse_plot, nldr1, nldr2,
                    nldr3, nldr4)

usethis::use_data(scurve_plts, overwrite = TRUE)
