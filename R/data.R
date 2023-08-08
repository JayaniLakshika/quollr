#' Generate Gaussian Clusters
#'
#' Generates a dataset with Gaussian clusters and additional random noise dimensions.
#'
#' @param n The number of observations in the dataset.
#' @param with_seed Optional. The seed value for reproducibility.
#' @param num_clusters The number of Gaussian clusters to generate.
#' @param mean_matrix The mean matrix for each cluster, where each row represents the mean vector for a cluster.
#'                    Default is rbind(c(1,0,0), c(0,1,0), c(0,0,1)).
#' @param var_vec The variance vector for each cluster. Default is c(0.05, 0.05, 0.05).
#' @param num_dims The number of dimensions for each observation. Should be at least 2. Default is 3.
#' @param num_noise_dims The number of additional random noise dimensions to add to the dataset. Default is 2.
#' @param min_noise The minimum value for the random noise dimensions. Default is -0.5.
#' @param max_noise The maximum value for the random noise dimensions. Default is 0.5.
#'
#' @return A tibble (data frame) with the generated dataset, including Gaussian clusters and random noise dimensions.
#'
#' @examples
#' data <- gaussian_clusters(n = 300, num_clusters = 3, mean_matrix = rbind(c(1,0,0), c(0,1,0), c(0,0,1)),
#'                           var_vec = c(0.05, 0.05, 0.05), num_dims = 3, num_noise_dims = 2,
#'                           min_noise = -0.5, max_noise = 0.5)
#'
#' @export
gaussian_clusters <- function(n = 300, with_seed = NULL, num_clusters = 3, mean_matrix = rbind(c(1,0,0), c(0,1,0), c(0,0,1)),
                              var_vec = c(0.05, 0.05, 0.05), num_dims = 3, num_noise_dims = 2,
                              min_noise = -0.05, max_noise = 0.05) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  if (n < num_clusters) {
    stop('Number of clusters exceed the number of observations.')

  }

  if ((num_dims == 0) | (num_dims == 1)) {
    stop('There should be at least two dimensions.')

  }

  if (dim(mean_matrix)[1] != length(var_vec)) {
    stop('The length of mean and variance vectors are different.')

  }

  if (dim(mean_matrix)[1] != num_clusters) {
    stop('There is not enough mean values for clusters.')

  }

  if (dim(mean_matrix)[2] != num_dims) {
    stop('There is not enough mean values for dimensions.')

  }

  if (length(var_vec) != num_clusters) {
    stop('There is not enough varaiance values for clusters.')

  }

  # To check that the assigned n is divided by three
  if ((n%%num_clusters) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(n/num_clusters)

  } else {
    cluster_size <- n/num_clusters
  }


  # To generate empty tibble
  column_names <- paste0(rep("x", num_dims), 1:num_dims)
  df <- tibble::tibble(!!!stats::setNames(rep(list(NULL), length(column_names)), column_names))

  for (i in 1:num_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_matrix |>
      tibble::as_tibble(.name_repair = "unique") |>
      dplyr::filter(dplyr::row_number() == i) |>
      unlist(use.names = FALSE)

    # To filter the variance values for specific cluster
    variance_val_for_cluster <- var_vec[i]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[column_names[j]]] <- stats::rnorm(cluster_size, mean = mean_val_for_cluster[j],
                                               sd = variance_val_for_cluster)

    }
    # To generate a tibble for a cluster
    df_cluster <- tibble::as_tibble(dim_val_list)

    df <- dplyr::bind_rows(df, df_cluster)

  }

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_noise_dims), (NCOL(df) + 1):((NCOL(df) + 1) + num_noise_dims))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_noise_dims) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(n,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(n,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

#' Generate S-curve Data with Additional Noise Dimensions
#'
#' Generates an S-curve dataset with additional random noise dimensions.
#'
#' @param n The number of observations in the dataset.
#' @param with_seed Optional. The seed value for reproducibility.
#' @param num_noise_dims The number of additional random noise dimensions to add to the dataset. Default is 8.
#' @param min_noise The minimum value for the random noise dimensions. Default is -0.5.
#' @param max_noise The maximum value for the random noise dimensions. Default is 0.5.
#'
#' @return A tibble (data frame) with the generated dataset, including an S-curve and random noise dimensions.
#'
#' @import snedata
#' @importFrom dplyr select
#' @examples
#' data <- s_curve_with_noise_dims(n = 200, num_noise_dims = 3,
#'                                 min_noise = -0.5, max_noise = 0.5)
#'
#' @export
s_curve_with_noise_dims <- function(n = 200, with_seed = NULL, num_noise_dims = 3,
                               min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  if (num_noise_dims < 1) {
    stop('The number of noise dimensions should be greater than 1.')

  }

  df <- snedata::s_curve(n_samples = n, noise = 0.05)
  df <- df |>
    dplyr::select(-color)
  names(df) <- paste0(rep("x",3), 1:3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_noise_dims), (NCOL(df) + 1):((NCOL(df) + 1) + num_noise_dims))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_noise_dims) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(n,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(n,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}


#' Generate Data with Triangular Plane and Background Noise
#'
#' Generate synthetic data points with a triangular plane and background noise. This function
#' creates a dataset with a triangular cluster in two dimensions and adds background noise to
#' the points. The number of data points and the number of noise dimensions can be controlled.
#'
#' @param sample_size Number of total data points.
#' @param with_seed Seed value for reproducibility. If NULL, no seed is set.
#' @param num_of_noise_dim Number of additional noise dimensions to be added to the data.
#' @param min_noise Minimum value for generating noise.
#' @param max_noise Maximum value for generating noise.
#'
#' @return A dataset with synthetic data points and background noise.
#' @export
#'
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble as_tibble
#' @importFrom utils globalVariables
#'
#' @examples
#' data <- triangular_plane_with_bkg_noise(sample_size = 675, with_seed = 42, num_of_noise_dim = 4,
#'                                         min_noise = -0.5, max_noise = 0.5)
#'
#' @export
triangular_plane_with_bkg_noise <- function(sample_size = 675, with_seed = NULL, num_of_noise_dim = 4,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")

  } else {
    cluster_size <- sample_size/3
  }


  trace.point <- runif(2)
  corner.points <- tibble::tibble(x1 =c(  0,  1, 0.5),
                                  x2 =c(  0,  0,   1))
  df1 <- tibble::tibble(x1 =rep(0,cluster_size),
                        x2 =rep(0,cluster_size))
  for(i in 1:cluster_size){
    trace.point    = (corner.points[sample(3,1),]+trace.point)/2
    df1[i,] = trace.point
  }


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- stats::runif(cluster_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(cluster_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- stats::rnorm(cluster_size, mean = 0.025, sd = 0.5)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2, -df1)

  df

}

two_curvilinear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 4,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- sample_size/2
  }


  theta = runif(cluster_size, 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + rnorm(cluster_size, 1, 0.06)
  )

  theta1 <- stats::runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = 1 + cos(theta1) + rnorm(cluster_size, 1, 0.06),
    x2 = 1 + sin(theta1) + rnorm(cluster_size, 1, 0.06),

    x3 = 1 + cos(theta1) + rnorm(cluster_size, 1, 0.06),
    x4 = 1 + sin(theta1) + rnorm(cluster_size, 1, 0.06)
  )

  df <- dplyr::bind_rows(df1, df2)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

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

two_curvilinear_diff_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 4,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  theta = runif(cluster_size, 0.40, 0.70 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + rnorm(cluster_size, 1, 0.06)
  )

  theta1 = runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = 1 + cos(theta1) + rnorm(cluster_size, 1, 0.06),
    x2 = 1 + sin(theta1) + rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta1) + rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta1) + rnorm(cluster_size, 1, 0.06)
  )

  df <- bind_rows(df1, df2)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_linear_diff_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")

  } else {
    cluster_size <- sample_size/3
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) |>
    dplyr::group_by(color) |>
    dplyr::group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 20
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- df_2_split[[1]]
  df_2_split_3$x <- df_2_split_3$x + 10
  df_2_split_3$y <- df_2_split_3$y + 10

  df <- dplyr::bind_rows(df_2_split_1, df_2_split[[2]], df_2_split_3) |>
    dplyr::select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

three_nonlinear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  phi <- runif(cluster_size, max = 2*pi)
  rho <- sqrt(runif(cluster_size))

  theta <- runif(cluster_size, 0,1.80 * pi)
  x <- theta
  y <- sin(theta)

  df1 <- tibble::tibble(x1=x, x2=y, x3=sqrt(1)*rho*cos(phi) + 4, x4=sqrt(1)*rho*sin(phi) + 4)
  df2 <- tibble::tibble(x1=x+1, x2=y+1, x3=sqrt(1)*rho*cos(phi) + 6, x4=sqrt(1)*rho*sin(phi) + 6)
  df3 <- tibble::tibble(x1=x-1, x2=y-1, x3=sqrt(1)*rho*cos(phi) + 8, x4=sqrt(1)*rho*sin(phi) + 8)

  df <- dplyr::bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

three_cluster_mirror_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%6) != 0) {
    stop("The sample size should be a product of 6.")

  } else {
    cluster_size <- sample_size/6
  }


  df1 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05))

  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05))

  df3 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 1, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05))

  df_1 <- dplyr::bind_rows(df1, df2, df3)

  df_2 <- df_1 + 2
  df <- dplyr::bind_rows(df_1, df_2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

mobius_cluster_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                      min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  mobius <- geozoo::mobius.experiment(p = 5, n = sample_size* 0.80)

  df1 <- tibble::as_tibble(mobius$points)

  names(df1) <- paste0(rep("x", length(names(df1))), 1: length(names(df1)))


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size* 0.80,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size* 0.80,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(sample_size * 0.20, mean = 0, sd = 0.3)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2)


  df

}

four_long_clusters_with_bkg_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                              min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%5) != 0) {
    stop("The sample size should be a product of 5.")

  } else {
    cluster_size <- sample_size/5
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) |>
    dplyr::group_by(color) |>
    dplyr::group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 20
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- df_2_split[[1]]
  df_2_split_3$x <- df_2_split_3$x - 10
  df_2_split_3$y <- df_2_split_3$y + 10

  df_2_split_4 <- df_2_split[[1]]
  df_2_split_4$x <- df_2_split_4$x + 20
  df_2_split_4$y <- df_2_split_4$y + 30

  df1 <- bind_rows(df_2_split_1, df_2_split[[2]], df_2_split_3, df_2_split_4) |>
    dplyr::select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(cluster_size * 4,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(cluster_size * 4,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(cluster_size, mean = 0, sd = 10)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2)

  df

}

curvy_branching_cluster_with_bkg_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                                   min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%4) != 0) {
    stop("The sample size should be a product of 4.")

  } else {
    cluster_size <- sample_size/4
  }


  theta <- runif(cluster_size, 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + rnorm(cluster_size, 1, 0.06)
  )

  theta1 <- runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = cos(-theta1) + rnorm(cluster_size, 1, 0.06),
    x2 = sin(-theta1) + rnorm(cluster_size, 1, 0.06),

    x3 = cos(-theta1) + rnorm(cluster_size, 1, 0.06),
    x4 = sin(-theta1) + rnorm(cluster_size, 1, 0.06)
  )


  df3 <- tibble::tibble(x1 = rnorm(cluster_size, mean = 1, sd = 0.08), x2 = rnorm(cluster_size, mean = 1, sd = 0.08), x3=rnorm(cluster_size, mean = 1, sd = 0.08), x4=rnorm(cluster_size, mean = 1, sd = 0.08))

  df4 <- tibble::tibble(x1 = rnorm(cluster_size, mean = 1, sd = 1), x2 = rnorm(cluster_size, mean = 1, sd = 1), x3=rnorm(cluster_size, mean = 1, sd = 1), x4=rnorm(cluster_size, mean = 1, sd = 1))


  df <- dplyr::bind_rows(df1, df2, df3, df4)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

s_curve_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                               min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  df <- snedata::s_curve(n_samples = sample_size, noise = 0.05)
  df <- df |>
    dplyr::select(-color)
  names(df) <- paste0(rep("x",3), 1:3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

three_linear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")

  } else {
    cluster_size <- sample_size/3
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) |>
    dplyr::group_by(color) |>
    dplyr::group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 20
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- df_2_split[[1]]
  df_2_split_3$x <- df_2_split_3$x - 10
  df_2_split_3$y <- df_2_split_3$y + 10

  df <- dplyr::bind_rows(df_2_split_1, df_2_split[[2]], df_2_split_3) |>
    dplyr::select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}


three_diff_linear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                         min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")

  } else {
    cluster_size <- sample_size/3
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) |>
    dplyr::group_by(color) |>
    dplyr::group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 250
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- tibble::tibble(x = -df_2_split[[1]]$y, y = df_2_split[[1]]$x)

  df <- dplyr::bind_rows(df_2_split_1, df_2_split[[2]], df_2_split_3) |>
    dplyr::select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

four_diff_long_clutsers_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                               min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by four
  if ((sample_size%%4) != 0) {
    stop("The sample size should be a product of 4.")

  } else {
    cluster_size <- sample_size/4
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) |>
    dplyr::group_by(color) |>
    dplyr::group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 150
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- tibble::tibble(x = df_2_split[[1]]$y - 70, y = -df_2_split[[1]]$x)

  df_2_split_4 <- tibble::tibble(x = df_2_split_3$x, y = df_2_split_3$y + 150)

  df <- dplyr::bind_rows(df_2_split_1, df_2_split[[2]], df_2_split_3, df_2_split_4) |>
    dplyr::select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_s_curves_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of 2.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  df1 <- snedata::s_curve(n_samples = cluster_size)
  df1 <- df1 |>
    dplyr::select(-color)
  names(df1) <- paste0(rep("x",3), 1:3)

  df2 <- tibble::tibble(x1 = -df1$x1 + 5, x2 = df1$x2 + 1, x3 = df1$x3 + 1)

  df <- dplyr::bind_rows(df1, df2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

plane_2D_with_hole <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2, min_noise = 0, max_noise = 1) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by four
  if ((sample_size%%4) != 0) {
    stop("The sample size should be a product of 4.")

  } else {
    cluster_size <- sample_size/4
  }

  u <- runif(cluster_size, min = 10, max = 30)
  v <- runif(cluster_size, min = 10, max = 20)
  x <- u + v - 10
  y <- v - u + 8

  df1 <- tibble::tibble(x1 = x, x2 = y)

  anchor <- c(1, 1)
  indices <- rowSums((sweep(df1, 2, anchor, `-`))) > 30
  df1 <- df1[indices, ]
  rownames(df1) <- NULL

  df2 <- tibble::tibble(x1 = -df1$x2 + 26, x2 = df1$x1 - 15)
  df3 <- tibble::tibble(x1 = df1$x2 + 30, x2 = -df1$x1 + 25)

  df <- dplyr::bind_rows(df1 - 10, df1 + 10, df2, df3)

  sample_size <- NROW(df)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  return(list(df = df, sample_size = sample_size))

}

mirror_s_curves_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- sample_size/2
  }


  df1 <- snedata::s_curve(n_samples = cluster_size)
  df1 <- df1 |>
    dplyr::select(-color)
  names(df1) <- paste0(rep("x",3), 1:3)

  df2 <- tibble::tibble(x1 = -df1$x1 + 2, x2 = df1$x2, x3 = df1$x3)

  df <- dplyr::bind_rows(df1, df2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

traingular_3D_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                     min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  trace.point <- runif(3)
  corner.points <- tibble::tibble(x1 =c(  0,  1, 0.5, 0.5),
                                  x2 =c(  0,  0,   1, 0.5),
                                  x3 =c(  0,  0,   0,   1))
  df <- tibble::tibble(x1 =rep(0,sample_size),
                       x2 =rep(0,sample_size),
                       x3 =rep(0,sample_size))
  for(i in 1:sample_size){
    trace.point    = (corner.points[sample(4,1),]+trace.point)/2
    df[i,] = trace.point
  }


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_grid_with_bkg_noise <- function(n_value = 10, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }



  df1 <- snedata::grid_data(n = n_value)
  df1 <- df1 |>
    dplyr::select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)

  df3 <- df1 + 5

  df1 <- dplyr::bind_rows(df1, df3)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  sample_size <- NROW(df1) + NROW(df1) * 0.6/2

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(NROW(df1),
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(NROW(df1),
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(sample_size * 0.6/2.6, mean = 3, sd = 5)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2)

  return(list(df = df, sample_size = sample_size))

}

one_grid_diff_with_bkg_noise <- function(n_value = 10, with_seed = NULL, num_of_noise_dim = 8,
                                         min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }



  df1 <- snedata::grid_data(n = n_value)
  df1 <- df1 |>
    dplyr::select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)

  df3 <- df1 + 3

  df1 <- dplyr::bind_rows(df1, df3)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  sample_size <- NROW(df1) + NROW(df1) * 0.6/2

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(NROW(df1),
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(NROW(df1),
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(sample_size * 0.6/2.6, mean = 3, sd = 5)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2)

  return(list(df = df, sample_size = sample_size))

}

three_grid_with_noise <- function(n_value = 19, with_seed = NULL, num_of_noise_dim = 8,
                                  min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  df1 <- snedata::grid_data(n = n_value)
  df1 <- df1 |>
    dplyr::select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)
  df1$x3 <- runif(nrow(df1), -0.01, 0.01)
  df1$x4 <- runif(nrow(df1), -0.01, 0.01)

  df2 <- snedata::grid_data(n = n_value)
  df2 <- df2 |>
    dplyr::select(-color)

  names(df2) <- paste0(rep("x",2), c(1, 3))
  df2$x2 <- runif(nrow(df2), -0.01, 0.01)
  df2$x4 <- runif(nrow(df2), -0.01, 0.01)
  df2 <- df2 |>
    dplyr::select(x1, x2, x3, x4)

  df3 <- snedata::grid_data(n = n_value)
  df3 <- df3 |>
    dplyr::select(-color)

  names(df3) <- paste0(rep("x",2), c(1, 4))
  df3$x2 <- runif(nrow(df3), -0.01, 0.01)
  df3$x3 <- runif(nrow(df3), -0.01, 0.01)
  df3 <- df3 |>
    dplyr::select(x1, x2, x3, x4)


  df <- dplyr::bind_rows(df1, df2, df3)

  sample_size <- NROW(df)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  return(list(df = df, sample_size = sample_size))

}

two_s_curve_hole_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }


  ## S curve with a hole
  df1 <- snedata::s_curve_hole(n_samples = sample_size/2, noise = 0) ## Should add more data because remove to create the hole
  df1 <- df1 |>
    dplyr::select(-color)
  names(df1) <- paste0(rep("x",3), 1:3)

  df2 <- df1 + 1

  df <- dplyr::bind_rows(df1, df2)

  sample_size <- NROW(df)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  return(list(df = df, sample_size = sample_size))

}

two_nonlinear_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                     min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- sample_size/2
  }


  x <- runif(cluster_size, -8, 1.5)
  y <- -(exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  z <- -(exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)
  w <- -(exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -8, 1.5)
  y <- 3 - (exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  z <- 3 - (exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)
  w <- 3 - (exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2)




  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_doublets_with_bkg_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by four
  if ((sample_size%%4) != 0) {
    stop("The sample size should be a product of 4.")

  } else {
    cluster_size <- sample_size/4
  }


  df1 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05))


  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05))

  df6_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df6_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df6 <- df6_new[samp,]


  df3 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 1, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05))

  df7_new <- (df1 + df3) / 2
  #get a sample of 10
  samp <- sample(nrow(df7_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df7 <- df7_new[samp,]

  df4 <- tibble::tibble(x1=rnorm(cluster_size * 0.6, mean = 0, sd = 0.5), x2=rnorm(cluster_size * 0.6, mean = 0, sd = 0.5), x3=rnorm(cluster_size * 0.6, mean = 0, sd = 0.5), x4=rnorm(cluster_size * 0.6, mean = 0, sd = 0.5))


  df <- dplyr::bind_rows(df1, df2, df3, df6, df7, df4)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

curvy_branching_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- sample_size/2
  }


  theta <- runif(cluster_size, 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + rnorm(cluster_size, 1, 0.06)
  )

  theta1 <- runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = cos(-theta1) + rnorm(cluster_size, 1, 0.06),
    x2 = sin(-theta1) + rnorm(cluster_size, 1, 0.06),

    x3 = cos(-theta1) + rnorm(cluster_size, 1, 0.06),
    x4 = sin(-theta1) + rnorm(cluster_size, 1, 0.06)
  )

  df <- dplyr::bind_rows(df1, df2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

one_doublets_with_bkg_noise <- function(sample_size = 250, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2.5
  if ((sample_size%%2.5) != 0) {
    stop("The sample size should be a product of 2.5.")

  } else {
    cluster_size <- sample_size/2.5
  }


  df1 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.05), x2 = rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.05))

  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4_new <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.2), x2 = rnorm(cluster_size, mean = 0, sd = 0.5), x3=rnorm(cluster_size, mean = 0.5, sd = 0.5), x4=rnorm(cluster_size, mean = 0.2, sd = 0.5),
                            x5=rnorm(cluster_size, mean = 0.2, sd = 0.3),
                            x6=rnorm(cluster_size, mean = 0, sd = 0.5),
                            x7=rnorm(cluster_size, mean = 0, sd = 0.3))

  #get a sample of 10
  samp1 <- sample(nrow(df4_new), cluster_size * 0.30) ## 20% from the original dataset

  #data in the sample
  df4 <- df4_new[samp1,]

  df <- dplyr::bind_rows(df1, df2, df3, df4)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_doublets_parallel_with_noise <- function(sample_size = 440, with_seed = NULL, num_of_noise_dim = 8,
                                             min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 4.4
  if (((sample_size * 10)%%44) != 0) { #sample_size%%4.4
    stop("The sample size should be a product of 4.4.")

  } else {
    cluster_size <- (sample_size * 10)/44
  }


  df1 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.05), x2 = rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=rnorm(cluster_size, mean = 0, sd = 0.05))

  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- tibble::tibble(x1=rnorm(cluster_size, mean = -1, sd = 0.05), x2 = rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = -1, sd = 0.05),
                        x8=rnorm(cluster_size, mean = -1, sd = 0.05),
                        x9=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=rnorm(cluster_size, mean = 0, sd = 0.05))

  df5 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = -1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = -1, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x8=rnorm(cluster_size, mean = -1, sd = 0.05),
                        x9=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=rnorm(cluster_size, mean = 0, sd = 0.05))

  df6_new <- (df4 + df5) / 2
  #get a sample of 10
  samp1 <- sample(nrow(df6_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df6 <- df6_new[samp1,]

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

one_doublet_dfifferent_pattern_clusters_with_noise <- function(sample_size = 280, with_seed = NULL, num_of_noise_dim = 8,
                                                               min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2.8
  if ((sample_size%%2.8) != 0) {
    stop("The sample size should be a product of 2.8.")

  } else {
    cluster_size <- sample_size/2.8
  }


  theta <- runif(cluster_size, 0.20, 0.60 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + rnorm(cluster_size, 1, 0.5),
    x2 = sin(theta) + rnorm(cluster_size, 1, 0.03),

    x3 = cos(theta) + rnorm(cluster_size, 1, 0.03),
    x4 = sin(theta) + rnorm(cluster_size, 1, 0.03),

    x5 = cos(theta) + rnorm(cluster_size, 1, 0.03),
    x6 = sin(theta) + rnorm(cluster_size, 1, 0.03),

    x7 = cos(theta) + rnorm(cluster_size, 1, 0.05),
    x8 = sin(theta) + rnorm(cluster_size, 1, 0.03),

    x9 = cos(theta) + rnorm(cluster_size, 1, 0.3),
    x10 = sin(theta) + rnorm(cluster_size, 1, 0.03))

  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.1), x2 = rnorm(cluster_size, mean = 0, sd = 0.08), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.08),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.08),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.08),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.02),
                        x9=rnorm(cluster_size, mean = 0, sd = 0.02),
                        x10=rnorm(cluster_size, mean = 0, sd = 0.02))


  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.80) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df <- dplyr::bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

one_doublet_dfifferent_var_clusters_with_noise <- function(sample_size = 260, with_seed = NULL, num_of_noise_dim = 8,
                                                           min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2.6
  if ((sample_size%%2.6) != 0) {
    stop("The sample size should be a product of 2.6.")

  } else {
    cluster_size <- sample_size/2.6
  }


  df1 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.1), x2 = rnorm(cluster_size, mean = 0, sd = 0.08), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.08),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.08),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.08),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.02),
                        x9=rnorm(cluster_size, mean = 0, sd = 0.02),
                        x10=rnorm(cluster_size, mean = 0, sd = 0.02))

  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.02), x2=rnorm(cluster_size, mean = 0, sd = 0.02), x3=rnorm(cluster_size, mean = 0, sd = 0.02), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 1, sd = 0.02),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.02),
                        x7=rnorm(cluster_size, mean = 0, sd = 0.02),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.02),
                        x9=rnorm(cluster_size, mean = 0, sd = 0.02),
                        x10=rnorm(cluster_size, mean = 0, sd = 0.02))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.60) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df <- bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

one_doublet_four_clusters_with_noise <- function(sample_size = 210, with_seed = NULL, num_of_noise_dim = 8,
                                                 min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 4.4
  if (((sample_size * 10)%%44) != 0) { #sample_size%%4.4
    stop("The sample size should be a product of 4.4.")

  } else {
    cluster_size <- (sample_size * 10)/44
  }


  df1 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2 = rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.05))

  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 1, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 0, sd = 0.05))


  df5 <- tibble::tibble(x1=rnorm(cluster_size, mean = 0, sd = 0.05), x2=rnorm(cluster_size, mean = 0, sd = 0.05), x3=rnorm(cluster_size, mean = 0, sd = 0.05), x4=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 0, sd = 0.05))

  df <- bind_rows(df1, df2, df3, df4, df5)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

three_doublets_with_noise <- function(sample_size = 210, with_seed = NULL, num_of_noise_dim = 8,
                                      min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 4.2
  if ((sample_size%%4.2) != 0) {
    stop("The sample size should be a product of number of clusters.")

  } else {
    cluster_size <- sample_size/4.2
  }


  df1 <- tibble::tibble(x1=rnorm(cluster_size, mean = 3, sd = 0.05), x2 = rnorm(cluster_size, mean = 1, sd = 0.05), x3=rnorm(cluster_size, mean = 1, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x10=rnorm(cluster_size, mean = 1, sd = 0.05))

  df2 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.05), x2=rnorm(cluster_size, mean = 1, sd = 0.05), x3=rnorm(cluster_size, mean = 1, sd = 0.05), x4=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x10=rnorm(cluster_size, mean = 1, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- tibble::tibble(x1=rnorm(cluster_size, mean = 1, sd = 0.05), x2=rnorm(cluster_size, mean = 1, sd = 0.05), x3=rnorm(cluster_size, mean = 1, sd = 0.05), x4=rnorm(cluster_size, mean = 3, sd = 0.05),
                        x5=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x7=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=rnorm(cluster_size, mean = 1, sd = 0.05),
                        x10=rnorm(cluster_size, mean = 1, sd = 0.05))

  df5_new <- (df2 + df4) / 2

  #get a sample of 10
  samp1 <- sample(nrow(df5_new), cluster_size * 0.30) ## 20% from the original dataset

  #data in the sample
  df5 <- df5_new[samp1,]

  df6_new <- (df1 + df4) / 2

  #get a sample of 10
  samp2 <- sample(nrow(df6_new), cluster_size * 0.50) ## 20% from the original dataset

  #data in the sample
  df6 <- df6_new[samp2,]

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

sphere_data_with_noise <- function(sample_size = 250, with_seed = NULL, num_of_noise_dim = 8,
                                   min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  df <- snedata::sphere(sample_size) |>
    dplyr::select(-color)

  names(df) <- paste0(rep("x", 3), 1:3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_curvilinear_data_with_noise <- function(sample_size = 250, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by two
  if (((sample_size - sample_size * 0.2)%%2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor((sample_size - sample_size * 0.2)/2)

  } else {
    cluster_size <- (sample_size - sample_size * 0.2)/2
  }




  x <- runif(cluster_size, -2, -0.5)
  y <- (x^2 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, 0.5, 2)
  y <- (x^2 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- rnorm(sample_size * 0.2, mean = 0, sd = 0.4)
  y <- rnorm(sample_size * 0.2, mean = 1.5, sd = 0.5)

  z <- rep(0, sample_size * 0.2) + rnorm(sample_size * 0.2, 10, 0.03)
  w <- rep(0, sample_size * 0.2) - rnorm(sample_size * 0.2, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

one_doublet_with_noise <- function(sample_size = 110, with_seed = NULL, num_of_noise_dim = 8,
                                   min_noise = -0.5, max_noise = 0.5) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }



  # # To check that the assigned sample_size is divided by 2.2
  # if ((sample_size%%2.2) != 0) {
  #   stop("The sample size should be a product of 2.2.")
  #
  # } else {
  #   cluster_size <- sample_size/2.2
  # }

  cluster_size <- sample_size/2.2



  df1 <- tibble::tibble(x=rnorm(cluster_size, mean = 0, sd = 0.05), y=rnorm(cluster_size, mean = 1, sd = 0.05), z=rnorm(cluster_size, mean = 0, sd = 0.05), w=rnorm(cluster_size, mean = 0, sd = 0.05))

  df2 <- tibble::tibble(x=rnorm(cluster_size, mean = 1, sd = 0.05), y=rnorm(cluster_size, mean = 0, sd = 0.05), z=rnorm(cluster_size, mean = 0, sd = 0.05), w=rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), sample_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]


  df <- dplyr::bind_rows(df1, df2, df3)
  df <- df |>
    dplyr::rename(x1 = x, x2 = y, x3 = z, x4 = w)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

eight_branching_data_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by eight
  if ((sample_size%%8) != 0) {
    stop("The sample size should be a product of 8.")

  } else {
    cluster_size <- sample_size/8
  }




  x <- runif(cluster_size, -1, 2)
  y <- (exp(x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 1)
  y <- (exp(2*x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 0.6)
  y <- (exp(3*x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 3)
  y <- (exp(0.5*x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 1)
  y <- (exp(-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 1)
  y <- (exp(2*-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df6 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.6, 1)
  y <- (exp(3*-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df7 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -3, 1)
  y <- (exp(0.5*-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df8 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}


four_branching_data_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                           min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 4
  if (((sample_size - sample_size * 0.1)%%4) != 0) {
    stop("The sample size should be a product of 4.")

  } else {
    cluster_size <- (sample_size - sample_size * 0.1)/4
  }




  x <- runif(cluster_size, -5, 1)
  y <- (exp(x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 5)
  y <- (exp(-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, 0, 5)
  y <- (log(x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -5, 0)
  y <- (log(-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(sample_size * 0.1, -5, 0)
  y <- runif(sample_size * 0.1, 0, 0.8) + runif(sample_size * 0.1, 0, 0.8)

  z <- rep(0, sample_size * 0.1) + rnorm(sample_size * 0.1, 10, 0.03)
  w <- rep(0, sample_size * 0.1) - rnorm(sample_size * 0.1, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

seven_branching_data_with_noise <- function(sample_size = 210, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by seven
  if ((sample_size%%7) != 0) {
    stop("The sample size should be a product of 7.")

  } else {
    cluster_size <- sample_size/7
  }



  x <- runif(cluster_size, -2, 2)
  y <- -(x^3 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 1.5)
  y <- (x^3 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 1.5)
  y <- (1 + (x-3)^2 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.1)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.5, 3)
  y <- (1 + -(x-3)^2 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.1)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 1)
  y <- (20 + x^3 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.01)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 2)
  y <- (x^2 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.01) + 10

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df6 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 2)
  y <- (x^2 + runif(cluster_size, 0, 0.2)) + runif(cluster_size, 0, 0.01) + 15

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df7 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5, df6, df7)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}


small_big_sphere_with_noise <- function(sample_size = 390, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 13
  if ((sample_size%%13) != 0) {
    stop("The sample size should be a product of 13.")

  } else {
    small_sphere_sample_size <- sample_size/13
  }


  df <- snedata::taspheres(n_samples = small_sphere_sample_size, d = 3, n_spheres = 4, r = 3) |>
    dplyr::select(-labels) # Creates a dataframe consisting of samples from the d-spheres of radius r enclosed within a larger d-sphere of radius 5 * r

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_curvy_panckakes_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                                           min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- sample_size/2
  }


  phi <- runif(cluster_size, max = 2*pi)
  rho <- sqrt(runif(cluster_size))

  theta <- runif(cluster_size, 0,1.80 * pi)
  x <- theta
  y <- sin(theta)

  df1 <- tibble::tibble(x1=x, x2=y, x3=sqrt(1)*rho*cos(phi) + 4, x4=sqrt(1)*rho*sin(phi) + 4)
  df2 <- tibble::tibble(x1=x+1, x2=y+1, x3=sqrt(1)*rho*cos(phi) + 6, x4=sqrt(1)*rho*sin(phi) + 6)


  df <- dplyr::bind_rows(df1, df2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

curvy_cell_cycle_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of three.")

  } else {
    cluster_size <- sample_size/3
  }


  r <- sqrt(3)/3

  theta = runif(cluster_size, 0, 2 * pi)
  x <- cos(theta)
  y <- r + sin(theta)
  z <- cos(3 * theta)/3

  df1 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- cos(theta) + 0.5
  y <- sin(theta) - r/2
  z <- cos(3 * theta)/3

  df2 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- cos(theta) - 0.5
  y <- sin(theta) - r/2
  z <- cos(3 * theta)/3

  df3 <- tibble::tibble(x1=x, x2=y, x3=z)

  df <- dplyr::bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

cell_cycle_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                                  min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")

  } else {
    cluster_size <- sample_size/3
  }


  r1 <- 2
  r2 <- 1

  theta = runif(cluster_size, 0, 2 * pi)
  x <- rep(0, cluster_size)
  y <- r1 * cos(theta)
  z <- r2 * sin(theta)

  df1 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- r2 * cos(theta)
  y <- rep(0, cluster_size)
  z <- r1 * sin(theta)

  df2 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- r1 * cos(theta)
  y <- r2 * sin(theta)
  z <- rep(0, cluster_size)

  df3 <- tibble::tibble(x1=x, x2=y, x3=z)

  df <- dplyr::bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

tree_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 5
  if ((sample_size%%5) != 0) {
    stop("The sample size should be a product of 5.")

  } else {
    cluster_size <- sample_size/5
  }


  x <- runif(cluster_size, -3, 3)
  y <- abs(0.5 * x)

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.5, 0.5)
  y <- abs(10*x)

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -6, 3)
  y <- (-1) * abs(0.5 * x + 5)

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.5, 0.5)
  y <- (-1) * abs(10 * x) - 5

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -5, 5)
  y <- x

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3, df4, df5)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

curvy_tree_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                                  min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")

  } else {
    cluster_size <- sample_size/3
  }


  x <- runif(cluster_size, -2, 2)
  y <- -(x^3 + runif(cluster_size, 0, 6)) + runif(cluster_size, 0, 0.2)

  z <- rnorm(cluster_size, 10, 0.1)
  w <- rnorm(cluster_size, 10, 0.1)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, 0, 2)
  y <- (x^3 + runif(cluster_size, 0, 6)) + runif(cluster_size, 0, 0.2)

  z <- rnorm(cluster_size, 10, 0.1)
  w <- rnorm(cluster_size, 10, 0.1)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 0)
  y <- -(x^3 + runif(cluster_size, 0, 6)) + runif(cluster_size, 0, 0.2) + 10

  z <- rnorm(cluster_size, 10, 0.1)
  w <- rnorm(cluster_size, 10, 0.1)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}


swiss_roll_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                  min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }


  df <- snedata::swiss_roll(n = sample_size)
  df <- df |>
    dplyr::select(-color)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

link_data_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                 min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- sample_size/2
  }


  df <- snedata::link_data(n = cluster_size)
  df <- df |>
    dplyr::select(-color)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

cluster_and_curvilinear__with_noise_and_bkg_noise <- function(sample_size = 260, with_seed = NULL, num_of_noise_dim = 8,
                                                              min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- (sample_size - sample_size * 0.3)/2
  }

  theta = runif(cluster_size, 0.20,0.60 * pi)
  x = cos(theta) + rnorm(cluster_size, 10, 0.03)
  y = sin(theta) + rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = rnorm(cluster_size, 10, 0.05)
  y = rnorm(cluster_size, 10, 0.05)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.05)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.05)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = rnorm(sample_size * 0.3, 11, 0.5)
  y = rnorm(sample_size * 0.3, 11, 0.5)

  z <- rep(0, sample_size * 0.3) + rnorm(sample_size * 0.3, 10, 0.05)
  w <- rep(0, sample_size * 0.3) - rnorm(sample_size * 0.3, 10, 0.05)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

three_circulars_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    stop("The sample size should be a product of 3.")

  } else {
    cluster_size <- sample_size/3
  }

  theta = runif(cluster_size, 0.0,2 * pi)
  x = cos(theta) + rnorm(cluster_size, 10, 0.03)
  y = sin(theta) + rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = 0.5 * cos(theta) + rnorm(cluster_size, 10, 0.03)
  y = 0.5 * sin(theta) + rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = rnorm(cluster_size, 10, 0.03)
  y = rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

nonlinear_mirror_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- sample_size/2
  }

  x <- runif(cluster_size, -8, 1.5)
  y <- -(exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -8, 1.5)
  y <- (exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

nonlinear_connect_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                         min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by four
  if ((sample_size%%4) != 0) {
    stop("The sample size should be a product of 4.")

  } else {
    cluster_size <- sample_size/4
  }

  theta = runif(cluster_size, 0,0.80 * pi)
  x = cos(theta) + rnorm(cluster_size, 10, 0.03)
  y = sin(theta) + rnorm(cluster_size, 10, 0.03)
  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)


  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = cos(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  y = sin(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = cos(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  z = sin(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  y <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = cos(theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  z = sin(theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  y <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- dplyr::bind_rows(df1, df2, df3, df4)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

long_cluster_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of two.")

  } else {
    cluster_size <- sample_size/2
  }



  df_2_split <- snedata::long_cluster_data(n = cluster_size) |>
    dplyr::group_by(color) |>
    dplyr::group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 20
  df_2_split_1$y <- df_2_split_1$y - 20

  df <- bind_rows(df_2_split_1, df_2_split[[2]]) |>
    dplyr::select(-color)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

s_curve_data_hole_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                         min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  df <- snedata::s_curve_hole(n_samples = sample_size, noise = 0) ## Should add more data because remove to create the hole
  df <- df |>
    dplyr::select(-color)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  sample_size_n <- NROW(df)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size_n,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size_n,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  return(list(df = df, sample_size = sample_size_n))

}

conic_spiral_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  conic_spiral <- geozoo::conic.spiral(n = sample_size, a = .2, b = 1, c = .1, w = 2)
  df <- tibble::as_tibble(conic_spiral$points, .name_repair = "unique")
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}


dini_surface_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  dini <- geozoo::dini.surface(n = sample_size, a = 1, b = 1)
  df <- tibble::as_tibble(dini$points, .name_repair = "unique")
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

mobius_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                              min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  mobius <- geozoo::mobius.experiment(p = 5, n = sample_size)
  df <- tibble::as_tibble(mobius$points, .name_repair = "unique")
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

roman_surface_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                     min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  klein <- geozoo::roman.surface(n = sample_size, a = 1)
  df <- tibble::as_tibble(klein$points, .name_repair = "unique")
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

spiral_with_noise <- function(sample_size = 100, with_seed = NULL, num_dims = 10, num_of_noise_dim = 8,
                              min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  result  <- mgc::mgc.sims.spiral(n = sample_size, d = num_dims)  # simulate 100 samples in 10 dimensions

  df <- tibble::as_tibble(result$X, .name_repair = "unique")
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
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
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}


three_clusters_data_with_noise <- function(sample_size = 100, with_seed = NULL, num_dims = 7, num_of_noise_dim = 8,
                                           min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size %% 3) != 0) {
    stop("The sample size should be a product of three.")

  } else {
    cluster_size <- sample_size/3
  }
  df <- snedata::three_clusters_data(n = cluster_size, dim = num_dims) ## n = number of points per Gaussian
  df <- df |>
    dplyr::select(-color)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

clusters_different_shapes <- function(sample_size = 300, with_seed = NULL, num_gussian_clusters = 4, num_non_gaussian_clusters = 2,
                                      cluster_sd_gau = 0.05, cluster_sd_non_gau = 0.1, num_dims = 7, a = 2, b = 4) {


  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  num_clusters <- num_gussian_clusters + num_non_gaussian_clusters

  # To check that the assigned sample_size is divided by number of clusters
  if ((sample_size%%num_clusters) != 0) {
    stop("The sample size should be a product of number of clusters.")

  } else {
    cluster_size <- sample_size/num_clusters
  }

  ## Generate Gaussian clusters

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- tidyr::expand_grid(!!!setNames(rep(list(values), num_dims),
                                                  paste0("mean_dim", 1:num_dims)))

  # To select combinations for assigned number of clusters

  mean_val_grid_gau <- mean_val_grid |>
    dplyr::slice_sample(n = num_gussian_clusters)

  mean_val_grid_non_gau <- mean_val_grid |>
    dplyr::slice_sample(n = num_non_gaussian_clusters)


  # To generate empty tibble
  column_names <- paste0(rep("x", num_dims), 1:num_dims)
  df <- tibble(!!!setNames(rep(list(NULL), length(column_names)), column_names))

  for (i in 1:num_gussian_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid_gau |>
      dplyr::filter(dplyr::row_number() == i) |>
      unlist(use.names = FALSE)

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[column_names[j]]] <- rnorm(cluster_size, mean = mean_val_for_cluster[j],
                                               sd = cluster_sd_gau)

    }
    # To generate a tibble for a cluster
    df_gau_cluster <- tibble::as_tibble(dim_val_list)

    df <- dplyr::bind_rows(df, df_gau_cluster)

  }

  phi <- runif(cluster_size, max = 2*pi)
  rho <- sqrt(runif(cluster_size))

  for (i in 1:num_non_gaussian_clusters) {

    # To filter the mean values for specific cluster
    presence_of_elipse_cluster <- mean_val_grid_non_gau |>
      dplyr::filter(dplyr::row_number() == i) |>
      unlist(use.names = FALSE)

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list_n <- list()

    for (j in 1:num_dims) {
      if(presence_of_elipse_cluster[j] == 1){
        dim_val_list_n[[column_names[j]]] <- sqrt(a)*rho*cos(phi) + b
        ## Surface of poolar coordinate
      } else {
        dim_val_list_n[[column_names[j]]] <- rnorm(cluster_size, mean = 0,
                                                   sd = cluster_sd_non_gau)

      }



    }
    # To generate a tibble for a cluster
    df_non_gau_cluster <- tibble::as_tibble(dim_val_list_n)

    df <- dplyr::bind_rows(df, df_non_gau_cluster)

  }

  df

}


nonlinear_2D <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2,
                         min_noise = -1, max_noise = 1) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  theta = runif(sample_size, 0.2, 0.6 * pi)
  x = cos(theta) + rnorm(sample_size, 10, 0.03)
  y = sin(theta) + rnorm(sample_size, 10, 0.03)

  df <- tibble::tibble(x1 = x, x2 = y)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
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
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  return(list(df = df, sample_size = sample_size))

}

curvilinear_2D <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2, min_noise = -1, max_noise = 1){
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  x <- runif(sample_size, 0, 2)
  y <- -(x^3 + runif(sample_size, 0, 3)) + runif(sample_size, 0, 0.5)

  df <- tibble::tibble(x1 = x, x2 = y)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

plane_2D <- function(sample_size = 100, with_seed = NULL, coefficient_x_1 = 1,
                     coefficient_x_2 = 1, coefficient_y_1 = -1, coefficient_y_2 = 1, intercept_x = -10,
                     intercept_y = 8, u_min = 10, u_max = 30, v_min = 10, v_max = 20, num_of_noise_dim = 2, min_noise = 0, max_noise = 1) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  u <- runif(sample_size, min = u_min, max = u_max)
  v <- runif(sample_size, min = v_min, max = v_max)
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
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

one_doublet_with_noise <- function(sample_size = 110, with_seed = NULL, num_of_noise_dim = 8,
                                   min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }


  # To check that the assigned sample_size is divided by 2.2
  if (((sample_size * 10)%%22) != 0) { #sample_size%%2.2
    stop("The sample size should be a product of 2.2.")

  } else {
    cluster_size <- (sample_size * 10)/22
  }


  df1 <- tibble::tibble(x=rnorm(cluster_size, mean = 0, sd = 0.05), y=rnorm(cluster_size, mean = 1, sd = 0.05), z=rnorm(cluster_size, mean = 0, sd = 0.05), w=rnorm(cluster_size, mean = 0, sd = 0.05))

  df2 <- tibble::tibble(x=rnorm(cluster_size, mean = 1, sd = 0.05), y=rnorm(cluster_size, mean = 0, sd = 0.05), z=rnorm(cluster_size, mean = 0, sd = 0.05), w=rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]


  df <- dplyr::bind_rows(df1, df2, df3)
  df <- df |>
    dplyr::rename(x1 = x, x2 = y, x3 = z, x4 = w)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}
