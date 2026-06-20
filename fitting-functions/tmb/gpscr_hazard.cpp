// Template to calculate the negative log-likelihood for a model with
// no random effects. NOT CURRENTLY USED IN THE PACKAGE.
#include <TMB.hpp>
#include <fenv.h>
#include "utilities.h"

template<class Type>
struct vec_list : vector<vector <Type> >  {
  vec_list(SEXP x){  /* x = List passed from R */
    (*this).resize(LENGTH(x));
    for(int i=0; i<LENGTH(x); i++){
      SEXP cm = VECTOR_ELT(x, i);
      (*this)(i) = asVector<Type>(cm);
    }
  }
};

template<class Type>
struct mat_list : vector<matrix <Type> >  {
  mat_list(SEXP x){  /* x = List passed from R */
    (*this).resize(LENGTH(x));
    for(int i=0; i<LENGTH(x); i++){
      SEXP cm = VECTOR_ELT(x, i);
      (*this)(i) = asMatrix<Type>(cm);
    }
  }
};

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Reading in data.
  DATA_INTEGER(n_sessions);
  DATA_IVECTOR(all_n_dets);
  DATA_STRUCT(all_capt, mat_list);
  DATA_STRUCT(all_bearing, mat_list);
  DATA_STRUCT(all_traps, mat_list);
  DATA_IVECTOR(all_n_traps);
  DATA_STRUCT(all_mask_dists, mat_list);
  DATA_STRUCT(all_mask_bearings, mat_list);
  DATA_IVECTOR(all_n_mask);
  DATA_VECTOR(all_mask_areas);
  DATA_STRUCT(all_X_mm, mat_list);
  DATA_STRUCT(all_Z_mm, mat_list);
  DATA_MATRIX(pred_X);
  DATA_MATRIX(pred_Z);
  DATA_IVECTOR(u_fac);
  DATA_INTEGER(do_smooth);
  DATA_INTEGER(do_bearing);
  DATA_INTEGER(n_occs);

  Type n_trials = n_occs;

  // Number of latent variables.
  int n_u = u_fac.size();
  // Parameters.
  PARAMETER_VECTOR(D_betas);
  PARAMETER(link_lambda0);
  Type lambda0 = exp(link_lambda0);
  PARAMETER(link_sigma);
  Type sigma = exp(link_sigma);
  PARAMETER(link_kappa);
  Type kappa = exp(link_kappa);
  // Standard deviation of the random effects.
  PARAMETER_VECTOR(link_sigma_u);
  vector<Type> sigma_u = exp(link_sigma_u);
  
  // Random effects.
  PARAMETER_VECTOR(u);
  // Setting a minimum value.
  double dbl_min = 1e-50;
  // Initiating the log-likelihood.
  Type f = 0;
  // Looping over sessions.
  for (int sess = 0; sess < n_sessions; sess++){
    // Extracting session-specific data.
    int n_dets = all_n_dets(sess);
    matrix<Type> capt = all_capt(sess);
    matrix<Type> traps = all_traps(sess);
    int n_traps = all_n_traps(sess);
    matrix<Type> mask_dists = all_mask_dists(sess);
    matrix<Type> bearing = all_bearing(0);
    matrix<Type> mask_bearings = all_mask_bearings(0);
    int n_mask = all_n_mask(sess);
    Type mask_area = all_mask_areas(sess);
    matrix<Type> X_mm = all_X_mm(sess);
    matrix<Type> Z_mm = all_Z_mm(sess);
    // Detection probabilities for mask/trap combinations.
    matrix<Type> prob_mat(n_mask, n_traps);
    // Overall detection probabilities for each mask point.
    vector<Type> prob_det(n_mask);
    // Calculating detection matrix.
    for (int i = 0; i < n_mask; i++){
      for (int j = 0; j < n_traps; j++){
	prob_mat(i, j) = 1 - exp(-lambda0 * exp((-pow(mask_dists(i, j), 2))/(2*pow(sigma, 2))));
      }
    }
    // Mask-level animal densities.
    vector<Type> D_mask = exp(X_mm*D_betas + Z_mm*u);
    // The sum of mask probabilities.
    Type sum_prob_det = 0;
    // The sum of the products of density and mask probabilities.
    Type sum_D_prob_det = 0;
    for (int i = 0; i < n_mask; i++){
      Type p_undet = Type(1);
      for (int j = 0; j < n_traps; j++){
	p_undet *= 1 - prob_mat(i, j);
      }
      prob_det(i) = 1 - pow(p_undet, n_trials);
      sum_prob_det += prob_det(i);
      sum_D_prob_det += D_mask(i)*prob_det(i);
    }
    // PMF for activity centres across the mask.
    vector<Type> f_loc(n_mask);
    f_loc = prob_det/sum_prob_det;
    // Likelihood contributions from capture histories.
    Type log_sum_integrands = 0;
    for (int i = 0; i < n_dets; i++){
      Type integrand = 0;
      for (int j = 0; j < n_mask; j++){
	Type integrand_mask = 0;
	for (int k = 0; k < n_traps; k++){
	  integrand_mask += dbinom_stable(capt(i, k), n_trials, prob_mat(j, k), true);
	  if (do_bearing){
	    if (capt(i, k) == 1){
	      integrand_mask += dvm_stable(bearing(i, k), mask_bearings(j, k), kappa, true);
	    }
	  }
	}
	integrand_mask += log(D_mask(j));
	integrand += exp(integrand_mask);
      }
      log_sum_integrands += log(integrand + dbl_min);
    }
    f-= log_sum_integrands;
    Type esa = mask_area*sum_prob_det;
    // Extra bit that falls out of log-likelihood.
    f -= -n_dets*log(sum_D_prob_det);
    // Contribution from number of animals detected.
    f -= dpois_stable(n_dets, mask_area*sum_D_prob_det, true);
  }
  // Contribution from random effects.
  if (do_smooth){
    for (int i = 0; i < n_u; i++){
      f -= dnorm(u(i), Type(0), sigma_u(u_fac(i) - 1), true);
    }
  }
  // Predicted values.
  vector<Type> log_D_pred = pred_X*D_betas + pred_Z*u;
  ADREPORT(log_D_pred);
  return f;
}
