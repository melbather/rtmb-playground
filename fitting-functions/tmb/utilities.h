#ifndef UTILITIES_H
#define UTILITIES_H

// Stable binomial PMF.
template<class Type>
Type dbinom_stable (const Type &k, const Type &size, const Type &prob, const int &give_log){
  Type out;
  out = exp(lgamma(size + 1) - lgamma(k + 1) - lgamma(size - k + 1))*pow(prob, k)*pow(1 - prob, size - k);
  if (give_log){
    out = log(out + DBL_MIN);
  }
  return out;
}

// Stable Poisson PMF.
template<class Type>
Type dpois_stable (const Type &x, const Type &lambda, const int &give_log){
  Type out;
  out = pow(lambda, x)*exp(-lambda)/exp(lgamma(x + 1));
  if (give_log){
    out = log(out + DBL_MIN);
  }
  return out;
}
template<class Type>
Type dpois_stable (const int &x, const Type &lambda, const int &give_log){
  Type d_x = x;
  Type out;
  out = pow(lambda, d_x)*exp(-lambda)/exp(lgamma(d_x + 1));
  if (give_log){
    out = log(out + DBL_MIN);
  }
  return out;
}

// Von-Mises PDF.
template<class Type>
Type dvm_stable(const Type &theta, const Type &mu, const Type &kappa, const int &give_log){
  Type out;
  out = kappa*(cos(theta - mu) - 1) - log(2*M_PI) - log(besselI(kappa, Type(0))/exp(kappa));
  if (give_log == 0){
    out = exp(out);
  }
  return out;
}

#endif
