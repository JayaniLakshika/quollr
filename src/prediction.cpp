// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Efficient nearest centroid lookup
// [[Rcpp::export]]
IntegerVector compute_highd_dist(NumericMatrix test_data, NumericMatrix centroids) {
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

// HBE and absolute error computation
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

  double hbe = std::sqrt(total_squared_error / n);
  return List::create(Named("Error") = total_abs_error, Named("HBE") = hbe);
}

// [[Rcpp::export]]
NumericVector calc_2d_dist_cpp(NumericVector x_from, NumericVector y_from,
                               NumericVector x_to, NumericVector y_to) {
  int n = x_from.size();
  NumericVector dist(n);

  for (int i = 0; i < n; ++i) {
    double dx = x_from[i] - x_to[i];
    double dy = y_from[i] - y_to[i];
    dist[i] = std::sqrt(dx * dx + dy * dy);
  }

  return dist;
}
