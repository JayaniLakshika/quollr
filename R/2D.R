#' Generate the Hexagonal
#'
#' @param .data
#' @param nldr_df
#' @param embedding_1
#' @param embedding_2
#' @param num_bins
#' @param shape_val
#' @param apply_pca
#'
#' @return an S4 object of class "hexbin".
#' @return hb: an S4 object of class "hexbin".
#' @export
#'
#' @examples
create_hexbin <- function(.data, nldr_df, embedding_1, embedding_2, num_bins = 30,
                          shape_val = 1, apply_pca = TRUE) {
  ### Merge tSNE dataset in 2D with original dataset which contains
  ### 4D coordinates
  .data <- .data |>
    dplyr::mutate(ID = dplyr::row_number())

  df_new <- .data |>
    dplyr::inner_join(nldr_df, by = "ID")

  ### Fit hexbins and store hexbin IDs
  hb <- hexbin::hexbin(nldr_df |>
                         dplyr::pull({
                           {
                             embedding_1
                           }
                         }), nldr_df |>
                         dplyr::pull({
                           {
                             embedding_2
                           }
                         }), xbins = num_bins, IDs = TRUE, shape = shape_val)
  ### Add hexbin Ids as a column to the original dataset

  df_new <- df_new |>
    dplyr::mutate(hb_id = hb@cID)

  ## To change column names to lower case
  names(df_new) <- tolower(names(df_new))

  ### Select specific columns to get the average number of 4D
  ### points in each bin
  if (isTRUE(apply_pca)) {
    ## Column names starts with pc
    df <- df_new |>
      dplyr::select(tidyselect::starts_with("pc"), hb_id)

  } else {
    ## Column names starts with x
    df <- df_new |>
      dplyr::select(tidyselect::starts_with("x"), hb_id)

  }

  return(list(df = df, hb = hb, df_new = df_new))
}

extract_hexbin_centroids <- function(.data, hb) {

  ### Computes x and y coordinates from hexagon cell id's
  xy <- hexbin::hcell2xy(hb)

  d_cell <- tibble::tibble(x_val_center = xy$x, y_val_center = xy$y)

  ### Data of each cell (bin) which contain ID as hex_bin ID
  df_cell_data1 <- tibble::tibble(ID = hb@cell, Cell_count = hb@count)

  df_cell_data <- dplyr::bind_cols(df_cell_data1, d_cell)

  df_cell_data <- df_cell_data |>
    dplyr::rename(hb_id = "ID")

  ### Merge hexbin data with original mean dataset
  df_b_with_center_data <- .data |>
    dplyr::inner_join(df_cell_data, by = "hb_id")

  df_b_with_center_data <- df_b_with_center_data |>
    dplyr::mutate(ID = dplyr::row_number())
  return(df_b_with_center_data)
}

triangulate_bin_centroids <- function(.data){
  tr1 <- tripack::tri.mesh(.data |> dplyr::pull(x_val_center), .data |> dplyr::pull(y_val_center))
  return(tr1)
}

generate_edge_info <- function(triangular_object) {
  tr_df <- tibble::tibble(x = triangular_object$x, y = triangular_object$y)  ## Create a dataframe with tri.mesh x and y coordinate values
  tr_df <- tr_df |>
    dplyr::mutate(ID = dplyr::row_number())  ## To add ID numbers, beacuse to join with from and to points in tri$arcs

  trang <- tripack::triangles(triangular_object)
  trang <- tibble::as_tibble(trang)

  tr_arcs_df1 <- tibble::tibble(from = trang$node1, to = trang$node2)  ## Create dataframe with from and to edges
  tr_arcs_df2 <- tibble::tibble(from = trang$node1, to = trang$node3)
  tr_arcs_df3 <- tibble::tibble(from = trang$node2, to = trang$node3)
  tr_arcs_df <- dplyr::bind_rows(tr_arcs_df1, tr_arcs_df2, tr_arcs_df3)  ## Create dataframe with from and to edges

  ## To obtain x and values of from to in a dataframe
  vec <- stats::setNames(rep("", 6), c("from", "to", "x_from", "y_from",
                                       "x_to", "y_to"))  ## Define column names
  # Initialize an empty dataframe to store data in a specific
  # format
  tr_from_to_df_coord <- dplyr::bind_rows(vec)[0, ]
  tr_from_to_df_coord <- tr_from_to_df_coord |>
    dplyr::mutate_if(is.character, as.numeric)

  for (i in 1:NROW(tr_arcs_df)) {
    from_row <- tr_df |>
      dplyr::filter(dplyr::row_number() == (tr_arcs_df |>
                                              dplyr::pull(from) |>
                                              dplyr::nth(i)))
    to_row <- tr_df |>
      dplyr::filter(dplyr::row_number() == (tr_arcs_df |>
                                              dplyr::pull(to) |>
                                              dplyr::nth(i)))
    tr_from_to_df_coord <- tr_from_to_df_coord |>
      tibble::add_row(from = from_row |>
                        dplyr::pull(ID), to = to_row |>
                        dplyr::pull(ID), x_from = from_row |>
                        dplyr::pull(x), y_from = from_row |>
                        dplyr::pull(y), x_to = to_row |>
                        dplyr::pull(x), y_to = to_row |>
                        dplyr::pull(y))  ## Add vector as an       appending row to the dataframe
  }

  return(tr_from_to_df_coord)
}

cal_2D_dist <- function(.data) {

  .data$distance <- lapply(seq(nrow(.data)), function(x) {
    start <- unlist(.data[x, c("x_from", "y_from")])
    end <- unlist(.data[x, c("x_to", "y_to")])
    sqrt(sum((start - end)^2))
  })

  distance_df <- .data |>
    dplyr::select("from", "to", "distance")

  distance_df$distance <- unlist(distance_df$distance)
  return(distance_df)
}

color_long_edges <- function(.data, benchmark_value, triangular_object,
                             distance_col) {
  tr_df <- tibble::tibble(x = triangular_object$x, y = triangular_object$y)

  tr_from_to_df_coord <- generate_edge_info(triangular_object)
  distance_df_small_edges <- .data |>
    dplyr::filter({
      {
        distance_col
      }
    } < benchmark_value)
  distance_df_long_edges <- .data |>
    dplyr::filter({
      {
        distance_col
      }
    } >= benchmark_value)

  distance_df_small_edges <- distance_df_small_edges |>
    dplyr::mutate(type = "small_edges")

  distance_df_long_edges <- distance_df_long_edges |>
    dplyr::mutate(type = "long_edges")

  distance_edges <- dplyr::bind_rows(distance_df_small_edges, distance_df_long_edges)

  tr_from_to_df_coord_with_group <- merge(tr_from_to_df_coord, distance_edges,
                                          by = c("from", "to"))


  ## To draw the tri.mesh plot using ggplot
  tri_mesh_plot <- ggplot2::ggplot(tr_df, aes(x = x, y = y)) + ggplot2::geom_segment(aes(x = x_from,
                                                                                         y = y_from, xend = x_to, yend = y_to, color = type), data = tr_from_to_df_coord_with_group) +
    ggplot2::geom_point() + ggplot2::coord_equal() + ggplot2::scale_colour_manual(values = c("#de2d26",
                                                                                             "#636363"))
  return(tri_mesh_plot)

}
