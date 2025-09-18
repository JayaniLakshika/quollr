#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame gen_hex_coord_cpp(IntegerVector hexID, NumericVector c_x, NumericVector c_y, double a1) {
  int n = hexID.size();
  int n_coords = 6;

  // Preallocate output vectors
  IntegerVector hex_poly_id(n * n_coords);
  NumericVector x(n * n_coords);
  NumericVector y(n * n_coords);

  double dx = a1 / 2.0;
  double dy = a1 / sqrt(3.0);
  double vf = a1 / (2.0 * sqrt(3.0));

  NumericVector x_add = NumericVector::create(0, -dx, -dx, 0, dx, dx);
  NumericVector y_add = NumericVector::create(dy, vf, -vf, -dy, -vf, vf);

  for (int i = 0; i < n; ++i) {
    double cx = c_x[i];
    double cy = c_y[i];
    int id = hexID[i];

    for (int j = 0; j < n_coords; ++j) {
      int index = i * n_coords + j;
      hex_poly_id[index] = id;
      x[index] = cx + x_add[j];
      y[index] = cy + y_add[j];
    }
  }

  return DataFrame::create(
    Named("h") = hex_poly_id,
    Named("x") = x,
    Named("y") = y
  );
}

