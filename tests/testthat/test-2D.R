test_that("gen_centroids() works", {

  all_centroids_df <- gen_centroids(nldr_obj = scurve_model_obj$nldr_obj,
                                    b1 = 4, q = 0.1)

  testthat::expect_snapshot(all_centroids_df)

})

test_that("gen_hex_coord() works", {

  width <- scurve_model_obj$hb_obj$a1
  all_centroids_df <- scurve_model_obj$hb_obj$centroids
  all_hex_coord <- gen_hex_coord(centroids_data = all_centroids_df, a1 = width)

  testthat::expect_snapshot(all_hex_coord)

})

test_that("assign_data() works", {

  all_centroids_df <- scurve_model_obj$hb_obj$centroids
  umap_with_hb_id <- assign_data(nldr_obj = scurve_model_obj$nldr_obj,
                                 centroids_data = all_centroids_df)

  testthat::expect_snapshot(umap_with_hb_id)

})

test_that("compute_std_counts() works", {

  umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
  std_count_df <- compute_std_counts(scaled_nldr_hexid = umap_with_hb_id)

  testthat::expect_snapshot(std_count_df)

})

test_that("find_pts() works", {

  umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id

  testthat::expect_snapshot(find_pts(scaled_nldr_hexid = umap_with_hb_id))

})

test_that("hex_binning() works", {

  testthat::expect_snapshot(hex_binning(nldr_obj = scurve_model_obj$nldr_obj,
                                        b1 = 4, q = 0.1))

})

test_that("find_non_empty_bins() works", {

  testthat::expect_snapshot(find_non_empty_bins(nldr_obj = scurve_model_obj$nldr_obj,
                                                m = 5))

  testthat::expect_error(find_non_empty_bins(nldr_obj = scurve_model_obj$nldr_obj,
                                             m = 21))

})

test_that("extract_hexbin_centroids() works", {

  all_centroids_df <- scurve_model_obj$hb_obj$centroids
  counts_data <- scurve_model_obj$hb_obj$std_cts

  testthat::expect_snapshot(extract_hexbin_centroids(centroids_data = all_centroids_df,
                                                     counts_data = counts_data))

})

test_that("extract_hexbin_mean() works", {

  all_centroids_df <- scurve_model_obj$hb_obj$centroids
  counts_data <- scurve_model_obj$hb_obj$std_cts
  umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id

  testthat::expect_snapshot(extract_hexbin_mean(data_hb = umap_with_hb_id,
                                                counts_data = counts_data,
                                                centroids_data = all_centroids_df))

})

test_that("tri_bin_centroids() works", {

  all_centroids_df <- scurve_model_obj$hb_obj$centroids
  counts_data <- scurve_model_obj$hb_obj$std_cts
  umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
  df_bin_centroids <- extract_hexbin_mean(data_hb = umap_with_hb_id,
  counts_data = counts_data, centroids_data = all_centroids_df)

  testthat::expect_snapshot(suppressWarnings(tri_bin_centroids(centroids_data = df_bin_centroids)))

})

test_that("cal_2d_dist() works", {

  tr_from_to_df <- scurve_model_obj$trimesh_data

  testthat::expect_snapshot(calc_2d_dist(trimesh_data = tr_from_to_df))

})

test_that("gen_edges() works", {

  all_centroids_df <- scurve_model_obj$hb_obj$centroids
  counts_data <- scurve_model_obj$hb_obj$std_cts
  umap_with_hb_id <- scurve_model_obj$hb_obj$data_hb_id
  df_bin_centroids <- extract_hexbin_centroids(counts_data = counts_data,
                                               centroids_data = all_centroids_df)
  suppressWarnings(tr1_object <- tri_bin_centroids(centroids_data = df_bin_centroids))

  testthat::expect_snapshot(gen_edges(tri_object = tr1_object,
                                      a1 = scurve_model_obj$hb_obj$a1))
})

test_that("update_trimesh_index() works", {

  tr_from_to_df <- scurve_model_obj$trimesh_data
  testthat::expect_snapshot(update_trimesh_index(trimesh_data = tr_from_to_df))

})
