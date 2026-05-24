# Wider function that calls the log likelihood function

source("misc functions/construct-design.R")

scr_likelihood <- function(design_matrix_data, full_data, homogeneous, formula = NULL) {

  # Parameters list
  if (homogeneous) {
    parameters <- list(
      log_D = log(0.1),
      logit_g0 = qlogis(0.5),
      log_sigma = log(100)
    )
    design <- NULL
  } else {
    design <- construct.design(as.formula(formula), df = design_matrix_data)
    X_fixed <- design$X.lbm
    X_random <- design$Z.lbm
    parameters <- list( 
      logit_g0 = qlogis(0.5),
      log_sigma = log(50),
      beta = rep(0, ncol(X_fixed)),
      u = rep(0, ncol(X_random))
    )
  }

  # Call SCR likelihood function
  get_scr_ll(parameters, full_data, homogeneous, design)

}

# Make the above into a closure so that it can be used by MakeADFun

scr_likelihood_closure <- function(f, mat_d, full_d, h, form) function(p) f(p, mat_d, full_d, h, form)