gaussian_clusters <- function(sample_size = 300, with_seed = NULL, num_clusters = 3,
                              cluster_sd = 0.05, num_dims = 3) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%num_clusters) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/num_clusters)

  } else {
    cluster_size <- sample_size/num_clusters
  }

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- tidyr::expand_grid(!!!stats::setNames(rep(list(values), num_dims),
                                                  paste0("mean_dim", 1:num_dims)))

  # To select combinations for assigned number of clusters

  mean_val_grid <- mean_val_grid %>%
    dplyr::slice_sample(n = num_clusters)


  # To generate empty tibble
  column_names <- paste0(rep("x", num_dims), 1:num_dims)
  df <- tibble::tibble(!!!stats::setNames(rep(list(NULL), length(column_names)), column_names))

  for (i in 1:num_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid %>%
      dplyr::filter(dplyr::row_number() == i) %>%
      unlist(use.names = FALSE)

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[column_names[j]]] <- stats::rnorm(cluster_size, mean = mean_val_for_cluster[j],
                                               sd = cluster_sd)

    }
    # To generate a tibble for a cluster
    df_cluster <- tibble::as_tibble(dim_val_list)

    df <- dplyr::bind_rows(df, df_cluster)

  }

  df

}

plane_2D <- function(sample_size = 100, with_seed = NULL, coefficient_x_1 = 1,
                     coefficient_x_2 = 1, coefficient_y_1 = -1, coefficient_y_2 = 1, intercept_x = -10,
                     intercept_y = 8, u_min = 10, u_max = 30, v_min = 10, v_max = 20, num_of_noise_dim = 2,
                     min_noise = 0, max_noise = 1) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  u <- stats::runif(sample_size, min = u_min, max = u_max)
  v <- stats::runif(sample_size, min = v_min, max = v_max)
  x <- coefficient_x_1 * u + coefficient_x_2 * v + intercept_x
  y <- coefficient_y_1 * u + coefficient_y_2 * v + intercept_y

  df <- tibble::tibble(x1 = x, x2 = y)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

curvilinear_2D <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2,
                           min_noise = -1, max_noise = 1) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  x <- stats::runif(sample_size, 0, 2)
  y <- -(x^3 + stats::runif(sample_size, 0, 3)) + stats::runif(sample_size, 0, 0.5)

  df <- tibble::tibble(x1 = x, x2 = y)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

cube_3D_with_noise <- function(with_seed = NULL, num_of_noise_dim = 2,
                               min_noise = -0.1, max_noise = 0.1) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  cube <- geozoo::cube.solid.grid(p = 3, n = 8)
  df <- tibble::as_tibble(cube$points, .name_repair = "unique")
  names(df) <- paste0(rep("x", 3), 1:3)

  sample_size <- NROW(df)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 4:(4 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  return(list(df = df, sample_size = sample_size))

}

nonlinear_2D <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2,
                         min_noise = -1, max_noise = 1) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  theta <- stats::runif(sample_size, 0.2, 0.6 * pi)
  x <- cos(theta) + stats::rnorm(sample_size, 10, 0.03)
  y <- sin(theta) + stats::rnorm(sample_size, 10, 0.03)

  df <- tibble::tibble(x1 = x, x2 = y)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

sine_curve_with_noise <- function(sample_size = 100, with_seed = NULL,
                                  num_of_noise_dim = 8, min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  theta <- stats::runif(sample_size, 0, 1.8 * pi)
  x <- theta
  y <- sin(theta)

  df <- tibble::tibble(x1 = x, x2 = y)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

three_clusters_data_with_noise <- function(sample_size = 100, with_seed = NULL,
                                           num_dims = 7, num_of_noise_dim = 8, min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }
  df <- snedata::three_clusters_data(n = cluster_size, dim = num_dims)  ## n = number of points per Gaussian
  df <- df |>
    select(-color)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) +
                                                                        1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

torus_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                             min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  torus <- geozoo::torus(p = 3, n = sample_size)
  df <- tibble::as_tibble(torus$points, .name_repair = "unique")
  names(df) <- paste0(rep("x", 3), 1:3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 4:(4 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}
