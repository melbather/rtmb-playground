# Function that creates parameters and design matrix for inhomogeneous 

source("misc functions/construct-design.R")

get_params <- function(design_matrix_data, formula, traps, detfn = "HN") {
  design <- construct.design(as.formula(formula), df = design_matrix_data)
  X <- design$X.lbm
  Z <- design$Z.lbm

  sigma <- avg_trap_dist(traps)

  if (detfn == "HN") {
    parameters <- list( 
      logit_g0 = qlogis(0.5),
      log_sigma = log(sigma),
      log_sigma_u = 1,
      beta = rep(0, ncol(X)),
      u = rep(0, ncol(Z))
    )
  } else if (detfn == "HHN") {
    parameters <- list( 
      log_lambda0 = log(50), # TODO figure out best way to get auto start val for lambda0
      log_sigma = log(sigma),
      log_sigma_u = log(1),
      beta = rep(0, ncol(X)),
      u = rep(0, ncol(Z))
    )
  }

  list(
    design_matrix = design,
    params = parameters,
    sm = design$sm,
    sm2ran = design$sm2ran
  )
}

