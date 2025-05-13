#' S-curve dataset with noise dimensions
#'
#' The `scurve` dataset contains a 3-dimensional S-curve with added noise dimensions.
#' Each data point is represented by seven dimensions (x1 to x7) and an ID.
#'
#' @format A data frame with 100 rows and 8 columns:
#' \describe{
#'   \item{ID}{Identification number}
#'   \item{x1, x2, x3, x4, x5, x6, x7}{High-dimensional coordinates}
#' }
#'
#' @examples
#' # Load the scurve dataset
#' data(scurve)
#'
#' # Display the first few rows of the dataset
#' head(scurve)
#'
#' @keywords datasets
#'
#' @rdname scurve
#' @docType data
#' @usage data(scurve)
#' @source This dataset is generated for illustrative purposes.
"scurve"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap dataset
#' data(scurve_umap)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap
#' @docType data
#' @usage data(scurve_umap)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap"

#' Predicted UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap_predict` dataset contains the predicted UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_predict`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, predicted first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, predicted second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap_predict dataset
#' data(scurve_umap_predict)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_predict)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_predict
#' @docType data
#' @usage data(scurve_umap_predict)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_predict"

#' Object for S-curve dataset
#'
#' The `scurve_model_obj` contains object of scaled umap embedding, x and y limits,
#' range of y; object of hexagonal binning information,
#' object of high-d model fitted to umap embedding for the training data,
#' the triangular object representing the triangulated bin centroids,
#' a tibble that contains the edge information, a tibble with
#' edge distance information.
#'
#' @format ## `scurve_model_obj`
#' An object of 6 elements
#' @examples
#' # Load the scurve_model_obj
#' data(scurve_model_obj)
#'
#' @keywords datasets
#'
#' @rdname scurve_model_obj
#' @docType data
#' @usage data(scurve_model_obj)
#'
#' @source This object is generated for illustrative purposes.
"scurve_model_obj"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap2` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap2`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap2 dataset
#' data(scurve_umap2)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap2)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap2
#' @docType data
#' @usage data(scurve_umap2)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap2"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap3` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap3`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap3 dataset
#' data(scurve_umap3)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap3)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap3
#' @docType data
#' @usage data(scurve_umap3)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap3"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap4` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap4`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap4 dataset
#' data(scurve_umap4)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap4)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap4
#' @docType data
#' @usage data(scurve_umap4)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap4"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap5` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap5`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap5 dataset
#' data(scurve_umap5)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap5)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap5
#' @docType data
#' @usage data(scurve_umap5)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap5"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap6` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap6`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap6 dataset
#' data(scurve_umap6)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap6)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap6
#' @docType data
#' @usage data(scurve_umap6)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap6"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap_mse` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_mse`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap_mse dataset
#' data(scurve_umap_mse)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_mse)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_mse
#' @docType data
#' @usage data(scurve_umap_mse)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_mse"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap_mse2` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_mse2`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap_mse2 dataset
#' data(scurve_umap_mse2)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_mse2)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_mse2
#' @docType data
#' @usage data(scurve_umap_mse2)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_mse2"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap_mse3` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_mse3`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap_mse3 dataset
#' data(scurve_umap_mse3)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_mse3)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_mse3
#' @docType data
#' @usage data(scurve_umap_mse3)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_mse3"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap_mse4` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_mse4`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap_mse4 dataset
#' data(scurve_umap_mse4)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_mse4)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_mse4
#' @docType data
#' @usage data(scurve_umap_mse4)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_mse4"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap_mse5` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_mse5`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap_mse5 dataset
#' data(scurve_umap_mse5)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_mse5)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_mse5
#' @docType data
#' @usage data(scurve_umap_mse5)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_mse5"

#' UMAP embedding for S-curve dataset which with noise dimensions
#'
#' The `scurve_umap_mse6` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_mse6`
#' A data frame with 75 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2D embeddings.}
#'   \item{ID}{Numeric, identifier for each data point.}
#' }
#' @examples
#' # Load the scurve_umap_mse6 dataset
#' data(scurve_umap_mse6)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_mse6)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_mse6
#' @docType data
#' @usage data(scurve_umap_mse6)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_mse6"

