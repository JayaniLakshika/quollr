// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Efficient nearest centroid lookup
// [[Rcpp::export]]
IntegerVector predict_emb_indices(NumericMatrix test_data, NumericMatrix centroids) {
  int n = test_data.nrow();    // Number of test points
  int m = centroids.nrow();    // Number of centroids
  int d = test_data.ncol();    // Dimensionality

  IntegerVector min_indices(n);

  for (int i = 0; i < n; i++) {
    double min_dist = R_PosInf;
    int min_j = -1;

    for (int j = 0; j < m; j++) {
      double dist = 0;
      for (int k = 0; k < d; k++) {
        double diff = test_data(i, k) - centroids(j, k);
        dist += diff * diff;
      }
      if (dist < min_dist) {
        min_dist = dist;
        min_j = j;
      }
    }

    min_indices[i] = min_j + 1;  // 1-based index
  }

  return min_indices;
}

// RMSE and absolute error computation
// [[Rcpp::export]]
List compute_errors(NumericMatrix true_data, NumericMatrix pred_data) {
  int n = true_data.nrow();
  int d = true_data.ncol();

  double total_abs_error = 0;
  double total_squared_error = 0;

  for (int i = 0; i < n; i++) {
    double abs_sum = 0;
    double squared_sum = 0;
    for (int j = 0; j < d; j++) {
      double diff = true_data(i, j) - pred_data(i, j);
      abs_sum += std::abs(diff);
      squared_sum += diff * diff;
    }
    total_abs_error += abs_sum;
    total_squared_error += squared_sum;
  }

  double rmse = std::sqrt(total_squared_error / n);
  return List::create(Named("Error") = total_abs_error, Named("RMSE") = rmse);
}
