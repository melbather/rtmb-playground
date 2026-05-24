# Function that creates parameters and design matrix for inhomogeneous 

source("misc functions/construct-design.R")

get_params <- function(design_matrix_data, full_data, formula) {
  design <- construct.design(as.formula(formula), df = design_matrix_data)
  X_fixed <- design$X.lbm
  X_random <- design$Z.lbm
  parameters <- list( 
    logit_g0 = qlogis(0.5),
    log_sigma = log(50),
    beta = rep(0, ncol(X_fixed)),
    u = rep(0, ncol(X_random))
  )

  list(
    design_matrix = design,
    params = parameters
  )
}


