#' Find Benchmark Value To Remove Long Edges
#'
#' This function finds the benchmark value to remove long edges based on the differences in a distance column.
#'
#' @param .data The data frame containing the distances.
#' @param distance_col The name of the column containing the distances.
#'
#' @return The benchmark value, which is the first largest difference in the distance column.
#'
#' @importFrom dplyr mutate across arrange pull nth bind_cols
#' @importFrom tibble tibble
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' tr1_object <- triangulate_bin_centroids(df_bin_centroids, x, y)
#' tr_from_to_df <- generate_edge_info(triangular_object = tr1_object)
#' distance_df <- cal_2d_dist(tr_from_to_df)
#' find_benchmark_value(.data = distance_df, distance_col = "distance")
#'
#' @export
find_benchmark_value <- function(.data, distance_col) {

  .data <- .data |>
    dplyr::mutate(dplyr::across({
      {
        distance_col
      }
    }, \(x) round(x, 3)))


  sorted_distance_df <- .data |>
    dplyr::arrange({
      {
        distance_col
      }
    })  ## Sort the distances

  unique_dist <- sorted_distance_df |>
    dplyr::pull({
      {
        distance_col
      }
    }) |>
    unique()  ## Get the unique distances

  dist_u <- tibble::tibble(unique_dist = unique_dist)
  dist_u <- dplyr::bind_cols(dist_u, rbind(NA, apply(dist_u, 2, diff)), .name_repair = "unique_quiet")  ## Calculate differences between unique distance
  names(dist_u)[2] <- "difference"

  dist_u <- dist_u |>
    dplyr::mutate(dplyr::across(difference, \(x) round(x, 4)))  ## For simplicity

  dist_u[is.na(dist_u)] <- 0  ## To replace missing values with zero

  benchmark_value_vec <- c()

  ## To find the first largest difference (Define a benchmark value
  ## to remove long edges)
  for (i in 1:dim(dist_u)[1]) {
    if(!is.na(dist_u$difference[i + 1])){
      if (dist_u$difference[i] > dist_u$difference[i + 1]) {
        if (!(is.na(dist_u$difference[i]))) {
          benchmark_value_vec[i] <- dist_u$difference[i]
          break
        }
      }
    }
  }

  benchmark_value_df <- dist_u[which(dist_u$difference == benchmark_value_vec[!(is.na(benchmark_value_vec))]),
                               1]  # To get the first value which contain large difference
  names(benchmark_value_df) <- "unique_dist"
  benchmark_value <- benchmark_value_df |>
    dplyr::pull(unique_dist) |>
    dplyr::nth(1)
  benchmark_value

}

#' Compute Mean Density of Hexagonal Bins
#'
#' This function calculates the mean density of hexagonal bins based on their neighboring bins.
#'
#' @param df_bin_centroids A data frame containing information about hexagonal bin centroids,
#' including the hexagon ID and the standard normalized counts (\code{std_counts}).
#' @param num_bins_x The number of bins along the x-axis for the hexagonal grid.
#'
#' @return A data frame with an additional column, \code{mean_density}, representing the mean
#' density of each hexagonal bin based on its neighboring bins.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' compute_mean_density_hex(df_bin_centroids, num_bins_x)
#'
#' @export
compute_mean_density_hex <- function(df_bin_centroids, num_bins_x) {

  # To store mean densities of hexagons
  mean_density_vec <- c()

  for (i in 1:length(df_bin_centroids$hexID)) {

    ## Identify neighbors of a specific hex bin
    neighbor_df <- df_bin_centroids |>
      dplyr::filter((hexID == (df_bin_centroids$hexID[i] + 1)) | (hexID == (df_bin_centroids$hexID[i] - 1)) |
                      (hexID == (df_bin_centroids$hexID[i] + (num_bins_x + 1))) |
                      (hexID == (df_bin_centroids$hexID[i] + num_bins_x)) |
                      (hexID == (df_bin_centroids$hexID[i] - (num_bins_x + 1))) |
                      (hexID == (df_bin_centroids$hexID[i] - num_bins_x)))

    mean_density <- neighbor_df |>
      dplyr::pull(std_counts) |>
      sum()/NROW(neighbor_df) ## The reason to take the mean is to check the density in a considerable amount

    mean_density_vec <- append(mean_density_vec, mean_density)

  }

  df_bin_centroids <- df_bin_centroids |>
    dplyr::mutate(mean_density = mean_density_vec)

  return(df_bin_centroids)

}


#' Find Low-Density Hexagons
#'
#' This function identifies hexagons with low density based on the mean density of their neighboring hexagons.
#'
#' @param df_bin_centroids_all The data frame containing all hexagonal bin centroids.
#' @param num_bins_x Number of bins along the x-axis for hexagon binning.
#' @param df_bin_centroids_low The data frame containing identified low-density hexagonal bin centroids.
#'
#' @return A vector containing the IDs of hexagons to be removed after investigating their neighboring bins.
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' df_bin_centroids_low <- df_bin_centroids |>
#' dplyr::filter(std_counts <= 0.2222222)
#' find_low_density_hexagons(df_bin_centroids_all = df_bin_centroids, num_bins_x = num_bins_x,
#' df_bin_centroids_low = df_bin_centroids_low)
#'
#'
#' @importFrom stats quantile
#'
#' @export
find_low_density_hexagons <- function(df_bin_centroids_all, num_bins_x, df_bin_centroids_low) {
  ## To compute mean density of hexagons
  df_bin_centroids <- compute_mean_density_hex(df_bin_centroids_all, num_bins_x)
  mean_density_vec <- df_bin_centroids$mean_density

  df_bin_centroids_low <- df_bin_centroids |>
    dplyr::filter(hexID %in% df_bin_centroids_low$hexID)

  ## Take first quartile
  benchmark_mean_dens_rm_hex <- stats::quantile(mean_density_vec, probs = c(0,0.25,0.5,0.75,1))[2]

  remove_bins <- c()

  ## Check only already identified low-density hexagons
  for (i in 1:length(df_bin_centroids_low$hexID)) {

    df_bin_centroids_coordinates_spec_bin <- df_bin_centroids_low |>
      dplyr::filter(hexID == df_bin_centroids_low$hexID[i])

    bin_ID <- df_bin_centroids_coordinates_spec_bin |>
      dplyr::pull(hexID)


    if(df_bin_centroids_coordinates_spec_bin$mean_density < benchmark_mean_dens_rm_hex){
      remove_bins <- append(remove_bins, bin_ID)
    }
  }

  return(remove_bins)
}


#' Extract Coordinates of Shifted Hexagonal Grid
#'
#' This function takes input data, which includes a hexbin ID, and extracts the coordinates
#' of a hexagonal grid with a specified shift. The resulting dataset includes hexagon centroids
#' with updated coordinates and additional information such as counts within each hexagon.
#'
#' @param nldr_data_with_hb_id A containing 2D embeddings with a hexbin ID.
#' @param num_bins_x The number of bins along the x-axis for the hexagonal grid.
#' @param hex_full_count_df A data frame with information about all hexagonal grid cells.
#' @param shift_x The value that centroids need to be shifted in x-axis.
#' To shift in the negative direction along the x-axis, set shift_x to a positive value (shift_x > 0);
#' otherwise, for a shift in the positive direction, use a negative value (shift_x < 0).
#' If not provided, it is calculated as half of the cell diameter of a hexagon.
#' @param shift_y The value that centroids need to be shifted in y-axis.
#' To shift in the negative direction along the y-axis, set shift_y to a positive value (shift_y > 0);
#' otherwise, for a shift in the positive direction, use a negative value (shift_y < 0).
#' If not provided, it is calculated as half of the cell diameter of a hexagon.
#' @param cell_area A numeric value that initialise the area of the hexagon. The default is 1.
#'
#' @return A data frame with updated hexagon coordinates, hexagon IDs, and counts within each hexagon.
#'
#' @return A list containing:
#'   \item{hex_full_count_df_new}{Data frame with updated hexagonal grid information, including counts and standardized counts.}
#'   \item{nldr_df_with_new_hexID}{Data frame with new hexagonal bin IDs assigned to 2D embeddings.}
#'
#' @examples
#' num_bins_x <- 4
#' shape_value <- 1.833091
#' hexbin_data_object <- extract_hexbin_centroids(nldr_df = s_curve_noise_umap,
#' num_bins = num_bins_x, shape_val = shape_value)
#' df_bin_centroids <- hexbin_data_object$hexdf_data
#' hex_full_count_df <- generate_full_grid_info(df_bin_centroids)
#' UMAP_data_with_hb_id <- s_curve_noise_umap |> dplyr::mutate(hb_id = hexbin_data_object$hb_data@cID)
#' extract_coord_of_shifted_hex_grid(nldr_data_with_hb_id = UMAP_data_with_hb_id,
#' num_bins_x = num_bins_x, hex_full_count_df)
#'
#' @export
extract_coord_of_shifted_hex_grid <- function(nldr_data_with_hb_id, num_bins_x,
                                              hex_full_count_df, shift_x = NA,
                                              shift_y = NA, cell_area = 1) {

  cell_diameter <- sqrt(2 * cell_area / sqrt(3))
  if (is.na(shift_x) | is.na(shift_y)) {
    shift_x <- cell_diameter/2
    shift_y <- cell_diameter/2

  }

  if ((abs(shift_x) > (cell_diameter/2)) | (abs(shift_y) > (cell_diameter/2))) {
    stop("Shifted amount is not compatibel. Need to use a value less than or equal 0.537285.")
  }

  ## Filter centroids with their hexIDs
  hexbin_coord_all <- hex_full_count_df |>
    dplyr::select(c_x, c_y, hexID) |>
    dplyr::distinct()

  hexbin_coord_all_new <- hexbin_coord_all |>
    dplyr::mutate(c_x = c_x - shift_x,
                  c_y = c_y - shift_y) |>
    dplyr::rename(c("x" = "c_x",
                    "y" = "c_y"))

  ## Generate all coordinates of hexagons
  hex_grid_new <- full_hex_grid(hexbin_coord_all_new)

  hexbin_coord_all_new <- hexbin_coord_all_new |>
    dplyr::rename(c("c_x" = "x",
                    "c_y" = "y"))

  ## Map the polygon ID to the hexagon coordinates
  full_grid_with_polygon_id_df <- map_polygon_id(hexbin_coord_all_new, hex_grid_new)

  full_grid_with_hexbin_id_rep <- full_grid_with_polygon_id_df |>
    dplyr::slice(rep(1:dplyr::n(), each = 6)) |>
    dplyr::arrange(polygon_id)

  ## Generate the dataset with polygon, and hexagon bin centroid coordinates
  hex_full_count_df_new <- dplyr::bind_cols(hex_grid_new, full_grid_with_hexbin_id_rep)

  ## Datafarme to store new hexIDs
  nldr_df_with_new_hexID <- data.frame(matrix(ncol = 0, nrow = 0))

  for (i in 1:NROW(nldr_data_with_hb_id)) {

    ## Select the nldr point
    nldr_data_with_hb_id_spec <- nldr_data_with_hb_id |>
      dplyr::filter(dplyr::row_number() == i)

    ## Find nearest hexIDs
    df_bin_centroids_coordinates_spec_bin_near1 <- hexbin_coord_all_new |>
      dplyr::filter((hexID == nldr_data_with_hb_id_spec$hb_id[1]) |(hexID == (nldr_data_with_hb_id_spec$hb_id[1] + (num_bins_x + 1))) | (hexID == (nldr_data_with_hb_id_spec$hb_id[1] + num_bins_x)) | (hexID == (nldr_data_with_hb_id_spec$hb_id[1] - (num_bins_x + 1))) | (hexID == (nldr_data_with_hb_id_spec$hb_id[1] - num_bins_x)))

    nldr_data_with_hb_id_spec <- nldr_data_with_hb_id_spec |>
      dplyr::select(-ID) |>
      dplyr::rename("x" = names(nldr_data_with_hb_id_spec)[1],
                    "y" = names(nldr_data_with_hb_id_spec)[2])

    df_bin_centroids_coordinates_spec_bin_near1 <- df_bin_centroids_coordinates_spec_bin_near1 |>
      dplyr::rename("x" = "c_x",
                    "y" = "c_y",
                    "hb_id" = "hexID")

    near_df_1 <- dplyr::bind_rows(nldr_data_with_hb_id_spec, df_bin_centroids_coordinates_spec_bin_near1)

    ## Compute the distance from selected point to neighbouring centroids
    near_df_1$distance <- lapply(seq(nrow(near_df_1)), function(x) {
      start <- unlist(near_df_1[1, c("x","y")])
      end <- unlist(near_df_1[x, c("x","y")])
      sqrt(sum((start - end)^2))})

    near_df_1$distance <- unlist(near_df_1$distance)

    near_df_1 <- near_df_1 |>
      dplyr::filter(dplyr::row_number() != 1) |>
      dplyr::arrange(distance)

    ## Select the most nearest centroid and assign the hexID of that centroid
    nldr_data_with_hb_id_spec <- nldr_data_with_hb_id_spec |>
      dplyr::select(-hb_id) |>
      dplyr::mutate(hb_id = near_df_1$hb_id[1])

    nldr_df_with_new_hexID <- dplyr::bind_rows(nldr_df_with_new_hexID, nldr_data_with_hb_id_spec)

  }


  ## Find counts within each hexagon
  hb_id_with_counts <- nldr_df_with_new_hexID |>
    dplyr::count(hb_id) |>
    dplyr::mutate(counts = n,
                  std_counts = n/max(n)) |>
    dplyr::select(-n)

  hex_full_count_df_new <- dplyr::left_join(hex_full_count_df_new, hb_id_with_counts,
                                            by = c("hexID" = "hb_id"))

  nldr_data_with_hb_id <- nldr_data_with_hb_id |>
    dplyr::select(-ID)

  names(nldr_df_with_new_hexID) <- names(nldr_data_with_hb_id)

  return(list(hex_full_count_df_new = hex_full_count_df_new,
              nldr_df_with_new_hexID = nldr_df_with_new_hexID))

}


