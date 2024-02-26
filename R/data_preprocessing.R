#' Scaling the 2D embeddings
#'
#' This function scales the 2D embeddings.
#'
#' @param nldr_df A data frame containing 2D embeddings.
#' @param x The name of the column that contains first 2D embeddings component.
#' @param y The name of the column that contains second 2D embeddings component.
#'
#' @return A list contains scaled 2D embeddings.
#'
#' @importFrom rlang as_string sym
#' @importFrom scales rescale
#'
#' @examples
#' generate_scaled_data(nldr_df = s_curve_noise_umap, x = "UMAP1", y = "UMAP2")
#'
#' @export
generate_scaled_data <- function(nldr_df, x = "UMAP1", y = "UMAP2") {

  ## Obtain 2D embeddings
  emb1_vec <- nldr_df[[rlang::as_string(rlang::sym(x))]]
  emb2_vec <- nldr_df[[rlang::as_string(rlang::sym(y))]]

  ## Scale first embedding between 0 and 1
  scaled_emb1_vec <- scales::rescale(emb1_vec)

  ## Scale second embedding between -sqrt(3)/2 and sqrt(3)/2
  y_min <- -sqrt(3)/2
  y_max <- sqrt(3)/2
  scaled_emb2_vec <- ((emb2_vec - min(emb2_vec))/ (max(emb2_vec) - min(emb2_vec))) * (y_max - y_min)

  scaled_emb_list <- list(scaled_emb1_vec = scaled_emb1_vec, scaled_emb2_vec = scaled_emb2_vec)

  ## Rename elements
  names(scaled_emb_list) <- paste0("scaled_", c(x, y))

  return(scaled_emb_list)
}
