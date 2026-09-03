# Function that creates parameters and design matrix for inhomogeneous 

source("./misc functions/construct-design.R")

get_params <- function(design_matrix_data, 
  formula, 
  traps, 
  num_species = 1, 
  num_fields = NULL,
  detfn = "HN") {
  
  design <- construct.design(as.formula(formula), df = design_matrix_data)
  X <- design$X.lbm
  Z <- design$Z.lbm

  # there probably is a cleaner way to do the below

  sigma <- avg_trap_dist(traps)

  if (num_species == 1) {
    beta <- rep(0, ncol(X))
    u <- rep(0, ncol(Z))
    log_sigma = log(sigma)
    log_sigma_u <- log(1) 

    if (detfn == "HN") {
      parameters <- list( 
        logit_g0 = qlogis(0.5),
        log_sigma = log_sigma,
        log_sigma_u = log_sigma_u,
        beta = beta,
        u = u
      )
    } else if (detfn == "HHN") {
      parameters <- list( 
        log_lambda0 = log(50),
        log_sigma = log_sigma,
        log_sigma_u = log_sigma_u,
        beta = beta,
        u = u
      )
    }

  } else {
    beta <- matrix(0, nrow = ncol(X) - 2, ncol = num_species)
    u <- matrix(0, nrow = ncol(Z), ncol = num_fields) 
    log_sigma_u <- rep(log(1), num_fields)
    log_sigma <- rep(log(sigma), num_species)

    if (detfn == "HN") {
      parameters <- list( 
        logit_g0 = rep(qlogis(0.5), num_species), 
        log_sigma = log_sigma,
        log_sigma_u = log_sigma_u,
        beta = beta,
        u = u
      )
    } else if (detfn == "HHN") {
      parameters <- list( 
        log_lambda0 = rep(log(50), num_species),
        log_sigma = log_sigma,
        log_sigma_u = log_sigma_u,
        beta = beta,
        u = u
      )
    }


  }

  list(
    design_matrix = design,
    params = parameters,
    sm = design$sm,
    sm2ran = design$sm2ran
  )
}
