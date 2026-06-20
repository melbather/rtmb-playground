#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix squarify(const NumericMatrix& mask, const NumericVector& D_mask)
{
  NumericVector x_mask = mask(_,0);
  NumericVector y_mask = mask(_,1);
  NumericVector x_unique = sort_unique(x_mask);
  NumericVector y_unique = sort_unique(y_mask);
  int n_x_unique = x_unique.size();
  int n_y_unique = y_unique.size();
  NumericMatrix z(n_x_unique, n_y_unique);
  std::fill(z.begin(), z.end(), NumericVector::get_na());
  int n_mask = x_mask.size();
  double x;
  double y;
  int x_index;
  int y_index;
  for (int i = 0; i < n_mask; i++){
    x = x_mask(i);
    y = y_mask(i);
    for (int j = 0; j < n_x_unique; j++){
      if (x == x_unique(j)){
	x_index = j;
	j = n_x_unique;
      }
    }
    for (int j = 0; j < n_y_unique; j++){
      if (y == y_unique(j)){
	y_index = j;
	j = n_y_unique;
      }
    }
    z(x_index, y_index) = D_mask(i);
  }
  return z;
}
