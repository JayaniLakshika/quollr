#' Compute maximum value of y for scaling
#'
#' This function compute the maximum y value need to use for scaling.
#'
#' @param aspect_ratio Numeric value representing the aspect ratio of the plot area.
#' @param hex_ratio Numeric value representing the ratio of the hexagon size.
#'
#' @return A value which should be used as maximum value of y when scaling.
#'
#'
#' @examples
#' calc_y_max(aspect_ratio = 2.019414, hex_ratio = 0.2309401)
#'
#' @export
calc_y_max <- function(aspect_ratio, hex_ratio) {

  if (is.na(aspect_ratio) ) {

    stop("Aspect ratio is missing.")

  }

  if (is.na(hex_ratio)) {

    stop("Hex ratio is missing.")
  }

  if (is.infinite(aspect_ratio)) {

    stop("Aspect ratio can't be infinite.")


  }

  if (is.infinite(hex_ratio)) {

    stop("Hex ratio can't be infinite.")


  }

  if (hex_ratio <= 0) {

    stop("Hex ratio can't be zero or negative.")


  }

  if (aspect_ratio <= 0) {

    stop("Aspect ratio can't be zero or negative.")

  }


  ymax <- ceiling(aspect_ratio/hex_ratio) * hex_ratio

  return(ymax)

}


#' Scaling the data
#'
#' This function scales the x and y coordinates.
#'
#' @param data A tibble or data frame.
#' @param x The name of the column that contains values along the x-axis.
#' @param y The name of the column that contains values along the y-axis.
#' @param hex_ratio Numeric value representing the ratio of the hexagon size.
#'
#' @return A list contains scaled x and y coordinates.
#'
#' @importFrom rlang as_string sym
#'
#' @examples
#' gen_scaled_data(data = s_curve_noise_umap, x = "UMAP1", y = "UMAP2")
#'
#' @export
gen_scaled_data <- function(data, x, y, hex_ratio = NA) {

  ## Obtain 2D embeddings
  emb1_vec <- data[[rlang::as_string(rlang::sym(x))]]
  emb2_vec <- data[[rlang::as_string(rlang::sym(y))]]

  ## Compute aspect ratio
  aspect_ratio <- abs(diff(range(emb2_vec))/diff(range(emb1_vec)))

  ## Scale first embedding between 0 and 1
  x_min <- 0
  x_max <- 1
  scaled_emb1_vec <- ((emb1_vec - min(emb1_vec))/
                        (max(emb1_vec) - min(emb1_vec))) * (x_max - x_min)

  ## Scale second embedding between 0 and ymax
  y_min <- 0

  if(is.na(hex_ratio)) {
    ## Default hex ratio
    hex_ratio <- 2/sqrt(3)
  }

  y_max <- calc_y_max(aspect_ratio = aspect_ratio, hex_ratio = hex_ratio)

  scaled_emb2_vec <- ((emb2_vec - min(emb2_vec))/
                        (max(emb2_vec) - min(emb2_vec))) * (y_max - y_min)

  scaled_emb_list <- list(scaled_emb1_vec = scaled_emb1_vec, scaled_emb2_vec = scaled_emb2_vec)

  ## Rename elements
  names(scaled_emb_list) <- paste0("scaled_", c(x, y))

  return(scaled_emb_list)
}

