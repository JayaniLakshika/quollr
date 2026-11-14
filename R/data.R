#' S-curve dataset with noise dimensions
#'
#' The `scurve` dataset contains a 3-dimensional S-curve with added noise dimensions.
#' Each data point is represented by seven dimensions (x1 to x7) and an ID.
#'
#' @format A data frame with 5000 rows and 8 columns:
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

#' UMAP embedding for `scurve` with n_neighbors = 15 and min_dist = 0.1
#'
#' The `scurve_umap` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap`
#' A data frame with 5000 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2-D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2-D embeddings.}
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

#' Predicted UMAP embedding for `scurve` data
#'
#' The `scurve_umap_predict` dataset contains the predicted UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap_predict`
#' A data frame with 5000 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, predicted first UMAP 2-D embeddings.}
#'   \item{UMAP2}{Numeric, predicted second UMAP 2-D embeddings.}
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
#' An object of five elements
#' \describe{
#'   \item{nldr_scaled_obj}{A tibble contains scaled first and second columns of NLDR
#'   data, and numeric vectors representing the limits of the original NLDR data.}
#'   \item{hb_obj}{A object that contains hexagonal binning information.}
#'   \item{model_highd}{A tibble with high-dimensional model.}
#'   \item{model_2d}{A tibble containing hexagonal bin centroids in 2-D}
#'   \item{trimesh_data}{A tibble that contains the edge information.}
#' }
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

#' UMAP embedding for `scurve` with n_neighbors = 10 and min_dist = 0.4
#'
#' The `scurve_umap2` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap2`
#' A data frame with 5000 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2-D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2-D embeddings.}
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

#' UMAP embedding for `scurve` with n_neighbors = 62 and min_dist = 0.1
#'
#' The `scurve_umap3` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap3`
#' A data frame with 5000 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2-D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2-D embeddings.}
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

#' UMAP embedding for `scurve` with n_neighbors = 30 and min_dist = 0.5
#'
#' The `scurve_umap4` dataset contains the UMAP (Uniform Manifold
#' Approximation and Projection) embeddings of a three-dimensional S-curve with
#' added noise. Each data point is represented by two UMAP coordinates (UMAP1
#' and UMAP2) and an ID.
#'
#' @format ## `scurve_umap4`
#' A data frame with 5000 rows and 3 columns:
#' \describe{
#'   \item{UMAP1}{Numeric, first UMAP 2-D embeddings.}
#'   \item{UMAP2}{Numeric, second UMAP 2-D embeddings.}
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

#' Summary with different number of bins for `scurve_umap`
#'
#' The `scurve_umap_rmse` dataset contains error, RMSE, b2, b, m, a1, a2,
#' and mean density (d_bar) for different number of bins in x-axis (b1).
#'
#' @format ## `scurve_umap_rmse`
#' A data frame with 70 rows and 10 columns:
#' \describe{
#'   \item{Error}{Numeric, model error.}
#'   \item{RMSE}{Numeric, Root Mean Square Error.}
#'   \item{b1}{Numeric, number of bins along x-axis.}
#'   \item{b2}{Numeric, number of bins along y-axis.}
#'   \item{b}{Numeric, number of total bins.}
#'   \item{m}{Numeric, number of non-empty bins.}
#'   \item{a1}{Numeric, hexagon bin width.}
#'   \item{a2}{Numeric, hexagon bin height.}
#'   \item{d_bar}{Numeric, mean desnity.}
#'   \item{method}{Character, NLDR method.}
#' }
#' @examples
#' # Load the scurve_umap_rmse dataset
#' data(scurve_umap_rmse)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_rmse)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_rmse
#' @docType data
#' @usage data(scurve_umap_rmse)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_rmse"

#' Summary with different number of bins for `scurve_umap2`
#'
#' The `scurve_umap_rmse2` dataset contains error, RMSE, b2, b, m, a1, a2,
#' and mean density (d_bar) for different number of bins in x-axis (b1).
#'
#' @format ## `scurve_umap_rmse2`
#' A data frame with 70 rows and 10 columns:
#' \describe{
#'   \item{Error}{Numeric, model error.}
#'   \item{RMSE}{Numeric, Root Mean Square Error.}
#'   \item{b1}{Numeric, number of bins along x-axis.}
#'   \item{b2}{Numeric, number of bins along y-axis.}
#'   \item{b}{Numeric, number of total bins.}
#'   \item{m}{Numeric, number of non-empty bins.}
#'   \item{a1}{Numeric, hexagon bin width.}
#'   \item{a2}{Numeric, hexagon bin height.}
#'   \item{d_bar}{Numeric, mean desnity.}
#'   \item{method}{Character, NLDR method.}
#' }
#' @examples
#' # Load the scurve_umap_rmse2 dataset
#' data(scurve_umap_rmse2)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_rmse2)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_rmse2
#' @docType data
#' @usage data(scurve_umap_rmse2)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_rmse2"

#' Summary with different number of bins for `scurve_umap3`
#'
#' The `scurve_umap_rmse3` dataset contains error, RMSE, b2, b, m, a1, a2,
#' and mean density (d_bar) for different number of bins in x-axis (b1).
#'
#' @format ## `scurve_umap_rmse3`
#' A data frame with 70 rows and 10 columns:
#' \describe{
#'   \item{Error}{Numeric, model error.}
#'   \item{RMSE}{Numeric, Root Mean Square Error.}
#'   \item{b1}{Numeric, number of bins along x-axis.}
#'   \item{b2}{Numeric, number of bins along y-axis.}
#'   \item{b}{Numeric, number of total bins.}
#'   \item{m}{Numeric, number of non-empty bins.}
#'   \item{a1}{Numeric, hexagon bin width.}
#'   \item{a2}{Numeric, hexagon bin height.}
#'   \item{d_bar}{Numeric, mean desnity.}
#'   \item{method}{Character, NLDR method.}
#' }
#' @examples
#' # Load the scurve_umap_rmse3 dataset
#' data(scurve_umap_rmse3)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_rmse3)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_rmse3
#' @docType data
#' @usage data(scurve_umap_rmse3)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_rmse3"

#' Summary with different number of bins for `scurve_umap4`
#'
#' The `scurve_umap_rmse4` dataset contains error, RMSE, b2, b, m, a1, a2,
#' and mean density (d_bar) for different number of bins in x-axis (b1).
#'
#' @format ## `scurve_umap_rmse4`
#' A data frame with 70 rows and 10 columns:
#' \describe{
#'   \item{Error}{Numeric, model error.}
#'   \item{RMSE}{Numeric, Root Mean Square Error.}
#'   \item{b1}{Numeric, number of bins along x-axis.}
#'   \item{b2}{Numeric, number of bins along y-axis.}
#'   \item{b}{Numeric, number of total bins.}
#'   \item{m}{Numeric, number of non-empty bins.}
#'   \item{a1}{Numeric, hexagon bin width.}
#'   \item{a2}{Numeric, hexagon bin height.}
#'   \item{d_bar}{Numeric, mean desnity.}
#'   \item{method}{Character, NLDR method.}
#' }
#' @examples
#' # Load the scurve_umap_rmse4 dataset
#' data(scurve_umap_rmse4)
#'
#' # Display the first few rows of the dataset
#' head(scurve_umap_rmse4)
#'
#' @keywords datasets
#'
#' @rdname scurve_umap_rmse4
#' @docType data
#' @usage data(scurve_umap_rmse4)
#'
#' @source This dataset is generated for illustrative purposes.
"scurve_umap_rmse4"


#' List of plots
#'
#' The `scurve_plts` contains the RMSE plot and the 2-D NLDR layouts for
#' `scurve_umap`, `scurve_umap2`, `scurve_umap3`, and `scurve_umap4`
#'
#' @format ## `scurve_plts`
#' A list of 5 elements:
#' \describe{
#'   \item{scurve_plts[[1]]}{ggplot object, RMSE plot.}
#'   \item{scurve_plts[[2]]}{ggplot object, 2-D NLDR layout for `scurve_umap`.}
#'   \item{scurve_plts[[3]]}{ggplot object, 2-D NLDR layout for `scurve_umap2`.}
#'   \item{scurve_plts[[4]]}{ggplot object, 2-D NLDR layout for `scurve_umap3`.}
#'   \item{scurve_plts[[5]]}{ggplot object, 2-D NLDR layout for `scurve_umap4`.}
#' }
#' @examples
#' # Show the scurve_plts
#' scurve_plts
#'
#' @keywords plots
#'
#' @rdname scurve_plts
#' @docType data
#'
#' @source This list of plots is generated for illustrative purposes.
"scurve_plts"


