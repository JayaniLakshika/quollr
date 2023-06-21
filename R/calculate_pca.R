#' Calculate Principal Components
#'
#' @param data - data set
#' @param num_pcs - number of pcs
#'
#' @return
#' @export
#'
#' @examples
calculate_pca <- function(data, num_pcs) {
  pcaY_cal <- stats::prcomp(data, center = TRUE, scale = FALSE)
  PCAresults <- data.frame(pcaY_cal$x[, 1:num_pcs])
  summary_pca <- summary(pcaY_cal)
  var_explained_df <- tibble::tibble(PC = paste0("PC",1:NCOL(data)),
                             var_explained = (pcaY_cal$sdev[1:NCOL(data)])^2/sum((pcaY_cal$sdev[1:NCOL(data)])^2))
  return(list(prcomp_out = pcaY_cal,pca_components = PCAresults, summary = summary_pca, var_explained_pca  = var_explained_df))

}

