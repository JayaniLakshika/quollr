#' S-curve dataset with noise dimensions
#'
#' The `s_curve_noise` dataset contains a 3-dimensional S-curve with added noise dimensions.
#' Each data point is represented by seven dimensions (x1 to x7) and an ID.
#'
#' @format A data frame with 100 rows and 8 columns:
#' \describe{
#'   \item{ID}{Identification number}
#'   \item{x1, x2, x3, x4, x5, x6, x7}{High-dimensional coordinates}
#' }
#'
#' @examples
#' # Load the s_curve_noise dataset
#' data(s_curve_noise)
#'
#' # Display the first few rows of the dataset
#' head(s_curve_noise)
#'
#' @keywords datasets
#'
#' @rdname s_curve_noise
#' @docType data
#' @usage data(s_curve_noise)
#' @source This dataset is generated for illustrative purposes.
"s_curve_noise"


#' S-curve dataset with noise dimensions for training
#'
#' The `s_curve_noise_training` dataset contains training data with dimensions x1,
#' x2, x3, x4, x5, x6, and x7. Each data point is identified by an ID.
#'
#' @format A data frame with 75 rows and 8 columns:
#' \describe{
#'   \item{ID}{Identification number}
#'   \item{x1, x2, x3, x4, x5, x6, x7}{High-dimensional coordinates}
#' }
#'
#' @examples
#' # Load the s_curve_noise_training dataset
#' data(s_curve_noise_training)
#'
#' # Display the first few rows of the dataset
#' head(s_curve_noise_training)
#'
#' @keywords datasets
#'
#' @rdname s_curve_noise_training
#' @docType data
#' @usage data(s_curve_noise_training)
#' @source This dataset is generated for training purposes.
"s_curve_noise_training"

#' S-curve dataset with noise dimensions for test
#'
#' The `s_curve_noise_test` dataset contains test data with dimensions x1,
#' x2, x3, x4, x5, x6, and x7. Each data point is identified by an ID.
#'
#' @format A data frame with 25 rows and 8 columns:
#' \describe{
#'   \item{ID}{Identification number}
#'   \item{x1, x2, x3, x4, x5, x6, x7}{High-dimensional coordinates}
#' }
#'
#' @examples
#' # Load the s_curve_noise_test dataset
#' data(s_curve_noise_test)
#'
#' # Display the first few rows of the dataset
#' head(s_curve_noise_test)
#'
#' @keywords datasets
#'
#' @rdname s_curve_noise_test
#' @docType data
#' @usage data(s_curve_noise_test)
#' @source This dataset is generated for training purposes.
"s_curve_noise_test"


#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `s_curve_noise_umap` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `s_curve_noise_umap`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the s_curve_noise_umap dataset
#' data(s_curve_noise_umap)
#'
#' # Display the first few rows of the dataset
#' head(s_curve_noise_umap)
#'
#' @keywords datasets
#'
#' @rdname s_curve_noise_umap
#' @docType data
#' @usage data(s_curve_noise_umap)
#'
#' @source This dataset is generated for illustrative purposes.
"s_curve_noise_umap"


#' Predicted UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `s_curve_noise_umap_predict` dataset contains the predicted UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `s_curve_noise_umap_predict`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, predicted first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, predicted second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the s_curve_noise_umap_predict dataset
#' data(s_curve_noise_umap_predict)
#'
#' # Display the first few rows of the dataset
#' head(s_curve_noise_umap_predict)
#'
#' @keywords datasets
#'
#' @rdname s_curve_noise_umap_predict
#' @docType data
#' @usage data(s_curve_noise_umap_predict)
#'
#' @source This dataset is generated for illustrative purposes.
"s_curve_noise_umap_predict"


#' Scaled UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `s_curve_noise_umap_scaled` dataset contains the scaled UMAP (Uniform Manifold
#' Approximation and Projection) embeddings.
#'
#' @format ## `s_curve_noise_umap_scaled`
#' A data frame with 25 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, Scaled first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, Scaled second UMAP 2D embedding.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the s_curve_noise_umap_scaled dataset
#' data(s_curve_noise_umap_scaled)
#'
#' # Display the first few rows of the dataset
#' head(s_curve_noise_umap_scaled)
#'
#' @keywords datasets
#'
#' @rdname s_curve_noise_umap_scaled
#' @docType data
#' @usage data(s_curve_noise_umap_scaled)
#'
#' @source This dataset is generated for illustrative purposes.
"s_curve_noise_umap_scaled"
