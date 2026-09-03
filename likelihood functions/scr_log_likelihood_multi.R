# adjustiing the likelihood function to work with two species

scr_log_likelihood_multi <- function(
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
  if (num_species > 1) {
    loading_matrix <- matrix(0, nrow = num_species, ncol = num_fields)
    num_non_zero <- sum(num_species - 1:num_fields)
    # finish this
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
        fixed_effects <- design_matrix[,1:nrow(beta)] %*% beta # I think it is breaking here now
        # matrix for each wiggly field
        wiggly_fields <- random_matrix %*% u
        spatial_effects <- wiggly_fields %*% loading_matrix
        D <- exp(fixed_effects + spatial_effects) / 1000 # matrix for all species to loop through below
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
        # error is here now - wrong dimensions
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
scr_log_likelihood_closure_multi <- function(f, d, h, design, m, df) function(p) f(p, d, h, design, m, df)
