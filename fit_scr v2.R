# Duplicating some fitting functions to test estimating diagonals of loading matrix

# Function that creates parameters and design matrix for inhomogeneous models

source("./misc functions/construct-design.R")

get_params2 <- function(
  design_matrix_data,
  formula,
  traps,
  num_species = 1,
  num_fields = NULL,
  detfn = "HN"
) {

  design <- construct.design(
    as.formula(formula),
    df = design_matrix_data
  )

  X <- design$X.lbm
  Z <- design$Z.lbm

  sigma <- avg_trap_dist(traps)


  if (num_species == 1) {
    beta <- rep(0, ncol(X))
    u <- rep(0, ncol(Z))
    log_sigma <- log(sigma)
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
    log_sigma <- rep(log(sigma), num_species)

    log_diag_loadings <- rep(0, num_fields)
    num_free_loadings <- sum(num_species - seq_len(num_fields))
    loading <- rep(0, num_free_loadings)

    if (detfn == "HN") {

      parameters <- list(
        logit_g0 = rep(qlogis(0.5), num_species),
        log_sigma = log_sigma,
        beta = beta,
        u = u,
        log_diag_loadings = log_diag_loadings,
        loading = loading
      )

    } else if (detfn == "HHN") {
      parameters <- list(
        log_lambda0 = rep(log(50), num_species),
        log_sigma = log_sigma,
        beta = beta,
        u = u,
        log_diag_loadings = log_diag_loadings,
        loading = loading
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


scr_log_likelihood_multi2 <- function(params, data_list, homogeneous, design, mask_dists, detfn = "HN") {
  getAll(params)

  # Detection parameters
  sigma <- exp(log_sigma)
  ADREPORT(sigma)

  if (detfn == "HN") {
    g0 <- plogis(logit_g0)
    ADREPORT(g0)
  } else if (detfn == "HHN") {
    lambda0 <- exp(log_lambda0)
    ADREPORT(lambda0)
  }

  num_species <- length(log_sigma)
  num_sessions <- length(data_list)

  # Random effects and loading matrix
  if (num_species == 1) {
    num_fields <- 1
    sigma_u <- exp(log_sigma_u)
    total_nll <- -sum(dnorm(u, mean = 0, sd = sigma_u, log = TRUE))
  } else {
    num_fields <- ncol(u)
    total_nll <- 0 

    for (k in seq_len(num_fields)) {
      total_nll <- total_nll - sum(dnorm(u[, k], mean = 0, sd = 1, log = TRUE))
    }

    diag_loadings <- exp(log_diag_loadings)
    loading_matrix <- matrix(0, nrow = num_species, ncol = num_fields)

    loading_index <- 1

    for (k in seq_len(num_fields)) {
      loading_matrix[k, k] <- diag_loadings[k]

      # loadings below diagonal
      if (k < num_species) {
        for (s in (k + 1):num_species) {
          loading_matrix[s, k] <- loading[loading_index]
          loading_index <- loading_index + 1
        }
      }
    }

    ADREPORT(diag_loadings)
    ADREPORT(loading_matrix)
  }

  total_ll <- 0 * sigma[1]
  curr_matrix_start <- 1
  tiny_num <- .Machine$double.xmin

  for (i in seq_len(num_sessions)) {
    data <- data_list[[i]]
    # Keep track of where we are in the design matrix
    num_rows <- nrow(data[[i]]$mask)
    matrix_range <- curr_matrix_start:(curr_matrix_start + num_rows - 1)
    design_matrix <- design$X.lbm[matrix_range,]
    random_matrix <- design$Z.lbm[matrix_range,]
    curr_matrix_start <- curr_matrix_start + num_rows


    if (homogeneous) {
      D <- exp(log_D) / 10000
    } else if (num_species == 1) {
      D <- as.vector(exp(design_matrix %*% beta + random_matrix %*% u) / 10000)
    } else {
      # D needs to be a matrix now, not a vector
      fixed_effects <- design_matrix[, seq_len(nrow(beta)), drop = FALSE] %*% beta
      # matrix for each wiggly field
      wiggly_fields <- random_matrix %*% u
      spatial_effects <- wiggly_fields %*% t(loading_matrix)
      D <- exp(fixed_effects + spatial_effects) / 10000
    }

    ## Number of traps.
    n.traps <- nrow(data$traps)
    ## Number of mask points.
    n.mask <- num_rows
    ## Area of a single mask cell
    a <- attr(data[[i]]$mask, "area")

    mask.dists <- mask_dists[[i]]

    # Loop over species
    for (s in seq_len(num_species)) {
      if (homogeneous) {
        # density for one species at a time
        D_species <- D[s]
      } else if (num_species == 1) {
        D_species <- D
      } else {
        # density for one species at a time
        D_species <- D[,s]
      }

      if (num_species == 1) {
        capt.hist <- as.matrix(data$binary_capture_history)
      } else {
        #browser()
        capt.hist <- as.matrix(data[[s]]$binary_capture_history)
      }

      n <- nrow(capt.hist)

      # detection function for this species
      # TODO make it so that each species could have a different type of detfn   
      if (detfn == "HN") {
        mask.probs <- g0[s] * exp(-mask.dists^2 / (2 * sigma[s]^2))
        p.avoid <- apply(1 - mask.probs, 1, prod)
        p.det <- 1 - p.avoid
        ## Calculating the effective sampling area.
        # make into vector
        esa <- a * sum(p.det)
        ADREPORT(esa)

        if (!homogeneous) {
          fs_denom <- D_species * p.det
          fs_denom_sum <- sum(fs_denom) * a
        }

      } else if (detfn == "HHN") {

        lambda <- lambda0[s] * exp(-mask.dists^2 / (2 * sigma[s]^2))
        p.det <- -expm1(-rowSums(lambda))
        esa <- a * sum(p.det)
        ADREPORT(esa)

        if (!homogeneous) {
          fs_denom <- D_species * p.det
          fs_denom_sum <- sum(fs_denom) * a
        }
      }

      if (n != 0) {

        if (detfn == "HHN") {
          log.f.capt.given.s <- log(-expm1(-lambda) + tiny_num) %*% t(capt.hist) - lambda %*% t(1 - capt.hist)
        } else {
          log.f.capt.given.s <- log(mask.probs + tiny_num) %*% t(capt.hist) + log(1 - mask.probs + tiny_num) %*% t(1 - capt.hist)
        }

        if (!homogeneous) {
          log.integrand <- log.f.capt.given.s + log(D_species) - log(fs_denom_sum)
        } else {
          log.integrand <- log.f.capt.given.s - log(esa)
        }
        f.capt <- colSums(exp(log.integrand) * a)
        log.f.capt <- sum(log(f.capt + tiny_num))
      } else {
        log.f.capt <- 0 
      }
      # Likelihood for the number of animals detected
      if (homogeneous) {
        log.f.n <- dpois(n, D_species * esa, log = TRUE)
      } else {
        log.f.n <- dpois(n, fs_denom_sum, log = TRUE)
      }

      ll <- log.f.n + log.f.capt
      total_ll <- total_ll + ll
    }
  }

  total_nll - total_ll
}

scr_log_likelihood_closure_multi2 <- function(f, d, h, design, m, df) function(p) f(p, d, h, design, m, df)

# VERSION 3 (estimate sigma_u and diag loadings) ---------------------------------

get_params3 <- function(
  design_matrix_data,
  formula,
  traps,
  num_species = 1,
  num_fields = NULL,
  detfn = "HN"
) {

  design <- construct.design(
    as.formula(formula),
    df = design_matrix_data
  )

  X <- design$X.lbm
  Z <- design$Z.lbm

  sigma <- avg_trap_dist(traps)


  if (num_species == 1) {
    beta <- rep(0, ncol(X))
    u <- rep(0, ncol(Z))
    log_sigma <- log(sigma)
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
    log_sigma <- rep(log(sigma), num_species)
    log_sigma_u <- log(1)

    log_diag_loadings <- rep(0, num_fields)
    num_free_loadings <- sum(num_species - seq_len(num_fields))
    loading <- rep(0, num_free_loadings)

    if (detfn == "HN") {
      parameters <- list( 
        logit_g0 = rep(qlogis(0.5), num_species), 
        log_sigma = log_sigma,
        log_sigma_u = log_sigma_u,
        beta = beta,
        u = u,
        log_diag_loadings = log_diag_loadings,
        loading = rep(0, sum(num_species - 1:num_fields))
      )
} else if (detfn == "HHN") {
  parameters <- list( 
    log_lambda0 = rep(log(50), num_species),
    log_sigma = log_sigma,
    log_sigma_u = log_sigma_u,
    beta = beta,
    u = u,
    log_diag_loadings = log_diag_loadings,
    loading = rep(0, sum(num_species - 1:num_fields))
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

scr_log_likelihood_multi3 <- function(
  params, 
  data_list, 
  homogeneous, 
  design, 
  mask_dists, 
  detfn = "HN"
) {
  getAll(params)
  # parameters
  sigma <- exp(log_sigma)
  ADREPORT(sigma)
  if (detfn == "HN") {
    g0 <- plogis(logit_g0)
    ADREPORT(g0)
  } else if (detfn == "HHN") {
    lambda0 <- exp(log_lambda0)
    ADREPORT(lambda0)
  }

  num_species <- length(log_sigma)
  num_fields <- length(log_sigma_u)
  num_sessions <- length(data_list)
  #browser()
  # random effects
  sigma_u <- exp(log_sigma_u)

  # NLL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (num_species == 1) {
    total_nll <- -sum(dnorm(u, mean = 0, sd = sigma_u, log = TRUE))
  } else {
    total_nll <- 0
    for (k in 1:num_fields) {
      total_nll <- total_nll - sum(dnorm(u[,k], mean = 0, sd = sigma_u[k], log = TRUE))
    }
  }

  print(total_nll)
  # Loading matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # number of rows = number of species
  # number of cols = number of fields
  # TODO check with Ben that this looks okay
  if (num_species > 1) {
  diag_loadings <- exp(log_diag_loadings)

  loading_matrix <- matrix(0, nrow = num_species, ncol = num_fields)

  loading_index <- 1

  for (k in 1:num_fields) {
    loading_matrix[k, k] <- diag_loadings[k]

    if (k < num_species) {
      for (s in (k + 1):num_species) {
        loading_matrix[s, k] <- loading[loading_index]
        loading_index <- loading_index + 1
      }
    }
  }

  ADREPORT(diag_loadings)
  ADREPORT(loading_matrix)
}
  
  curr_matrix_start <- 1
  num_sessions <- length(data_list)
  tiny_num <- .Machine$double.xmin
  for (i in 1:num_sessions) {
    data <- data_list[[i]]
    #browser()
    # Keep track of where we are in the design matrix
    num_rows <- nrow(data[[i]]$mask)
    matrix_range <- curr_matrix_start:(curr_matrix_start + num_rows - 1)
    design_matrix <- design$X.lbm[matrix_range,]
    random_matrix <- design$Z.lbm[matrix_range,]
    curr_matrix_start <- curr_matrix_start + num_rows

    # D differs based on whether we are using homogeneous or inhomogeneous
    if (homogeneous) {
      D <- exp(log_D)/10000
    } else {
      if (num_species == 1) {
        D <- as.vector(exp(design_matrix %*% beta + random_matrix %*% u)/10000)
      } else {
        # D needs to be a matrix now, not a vector
        fixed_effects <- design_matrix[,1:nrow(beta)] %*% beta 
        # matrix for each wiggly field
        wiggly_fields <- random_matrix %*% u
        spatial_effects <- wiggly_fields %*% loading_matrix
        D <- exp(fixed_effects + spatial_effects) / 10000 # matrix for all species to loop through below
      }
    } 

    ## Number of traps.
    n.traps <- nrow(data$traps)
    ## Number of mask points.
    n.mask <- num_rows
    ## Area of a single mask cell
    a <- attr(data[[i]]$mask, "area")

    mask.dists <- mask_dists[[i]]
    
    # loop over species
    total_ll <- 0
    for (s in 1:num_species) {
      if (homogeneous) {
        # density for one species at a time
        D_species <- exp(log_D[s]) / 1000
      } else {
        # density for one species at a time
        D_species <- D[,s]
      }

      # get the capt hist for the species subsetted above
      if (num_species == 1) {
        capt.hist <- as.matrix(data$binary_capture_history)
      } else {
        capt.hist <- as.matrix(data[[s]]$binary_capture_history)
      }

      # get n for this species
      n <- nrow(capt.hist)

      # detection function for this species
      # TODO make it so that each species could have a different type of detfn
      if (detfn == "HN") {
        mask.probs <- g0[s]*exp(-mask.dists^2/(2*sigma[s]^2))
        p.avoid <- apply(1 - mask.probs, 1, prod)
        p.det <- 1 - p.avoid
        ## Calculating the effective sampling area.
        # make into vector
        esa <- a*sum(p.det)
        ADREPORT(esa)

        # For inhomogeneous, calculate intensity
        if (!homogeneous) {
          fs_denom <- D_species*p.det
          fs_denom_sum <- sum(fs_denom) * a
        } 
      } else if (detfn == "HHN") {
        # Computing mask.probs here is problematic because R sometimes rounds some of these to 1
        lambda <- lambda0[s]*exp(-mask.dists^2/(2*sigma[s]^2))
        ## Calculating the effective sampling area.
        esa <- a*sum(1 - exp(-lambda))
        ADREPORT(esa)

        # For inhomogeneous, calculate intensity
        if (!homogeneous) {
          fs_denom <- D_species*(1 - exp(-lambda[s]))
          fs_denom_sum <- sum(fs_denom) * a
        } 
      }

      # likelihood for this species
      #Calculating likelihood contribution due to each detected animal's capture history.
      if (n != 0) {
        if (detfn == "HHN") log.f.capt.given.s <- log(-expm1(-lambda[s]) + tiny_num) %*% t(capt.hist) - lambda %*% t(1-capt.hist)
        else log.f.capt.given.s <- log(mask.probs + tiny_num) %*% t(capt.hist) + log(1-mask.probs) %*% t(1-capt.hist)

        if (!homogeneous) {
          # log.f.s <- log(D) + log(p.det + tiny_num) - log(fs_denom_sum) # realised this isn't actually used anywhere... do I still need it?
          log.integrand <- log.f.capt.given.s + log(D_species) - log(fs_denom_sum)
        } else {
          log.integrand <- log.f.capt.given.s - log(esa)
        }
          f.capt <- colSums(exp(log.integrand) * a)
          log.f.capt <- sum(log(f.capt + tiny_num))
      } else {
        log.f.capt <- 0
      }

      ## Log-likelihood contribution from the number of animals detected.
      if (homogeneous) {
        log.f.n <- dpois(n, D_species*esa, log = TRUE) 
      } else {
        log.f.n <- dpois(n, fs_denom_sum, log = TRUE)
      }
      ## Overall log-likelihood. The last part accounts for the fact that we cannot observe an all-zero capture history.
      ll <- log.f.n + log.f.capt
      total_ll <- total_ll + ll
    } 
  }
  #browser()
  total_nll - total_ll
}

# Make the above into a closure so that it can be used by MakeADFun
scr_log_likelihood_closure_multi3 <- function(f, d, h, design, m, df) function(p) f(p, d, h, design, m, df)


fit_scr3 <- function(
  bin_capt, 
  mask_coords, 
  mask_df, 
  traps, 
  formula, 
  num_species = 1,
  num_fields = NULL,
  detfn = "HN", 
  sigma_upper_limit = NULL) {
  # Load functions
  source("misc functions/avg_trap_dist.R")
  source("likelihood functions/scr_log_likelihood.R")
  source("likelihood functions/scr_log_likelihood_multi.R")
  source("misc functions/mask_cell_area.R")

  data <- list()
  # Consolidate data
  if (num_species == 1) {   
    for (i in 1:length(bin_capt)) { 
      data[[i]] <- list(
        binary_capture_history = as.data.frame(bin_capt[[i]]),
        traps = traps[[i]],
        mask = mask_coords[[i]]
      )
    }
  } else {
    for (i in 1:length(bin_capt)) { 
      data[[i]] <- list()
      for (j in 1:length(bin_capt[[i]])) {
        data[[i]][[j]] <- list(
          binary_capture_history = as.data.frame(bin_capt[[i]][[j]]),
          traps = traps[[i]],
          mask = mask_coords[[i]]
        )
      }
    }
  }

  # Calculate masks dists
  mask_dists <- list()
  for (i in 1:length(mask_coords)) {
    mask_dists[[i]] <- crossdist(mask_coords[[i]][, 1], mask_coords[[i]][, 2],
                              traps[[i]][, 1], traps[[i]][, 2])
  }

  # Get starting params and design matrix
  params_and_design <- get_params3(mask_df, formula, traps, num_species, num_fields, detfn)

  # Fit the model using likelihood function closure and RTMB
  # TODO get rid of this if/else block and just use one likelihood function
  if (num_species == 1) {
    obj_scr <- RTMB::MakeADFun(
    scr_log_likelihood_closure(
      scr_log_likelihood, 
      data, 
      FALSE, 
      params_and_design$design_matrix,
      mask_dists,
      detfn
    ), 
    params_and_design$params, 
    random = "u")
  } else {
    obj_scr <- RTMB::MakeADFun(
      scr_log_likelihood_closure_multi3(
        scr_log_likelihood_multi2, 
        data, 
        FALSE, 
        params_and_design$design_matrix,
        mask_dists,
        detfn
      ), 
    params_and_design$params, 
    random = "u")
  }

  if (!is.null(sigma_upper_limit)) {
    opt_scr <- nlminb(obj_scr$par, obj_scr$fn, obj_scr$gr, 
      upper = c(Inf, log(sigma_upper_limit), Inf, Inf, Inf, Inf))
  } else {
    opt_scr <- nlminb(obj_scr$par, obj_scr$fn, obj_scr$gr)
  }
  sdreport <- sdreport(obj_scr)

  # Return model, starting params, design matrix, opt_scr, and sdreport
  list(
    fit = obj_scr,
    start_pars = params_and_design$params,
    design_matrix = params_and_design$design_matrix,
    sm = params_and_design$sm,
    sm2ran = params_and_design$sm2ran,
    opt_scr = opt_scr,
    sdreport = sdreport,
    orig_mask = mask_df,
    model = formula,
    capture_hist = bin_capt
  )

}