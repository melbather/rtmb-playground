# This is where the likelihood function lives now because the main script was getting too crowded.

scr_log_likelihood <- function(params, data_list, homogeneous, design) {
  getAll(params)

  # parameters
  g0 <- plogis(logit_g0)
  sigma <- exp(log_sigma)
  # random effects
  sigma_u <- exp(log_sigma_u)

  ADREPORT(esa)
  ADREPORT(g0)
  ADREPORT(sigma)
  
  total_ll <- 0
  num_sessions <- length(data_list)
  curr_matrix_start <- 1

  for (i in 1:num_sessions) {
    # TODO Sunday - vectorise this
    data <- data_list[[i]]

    # Keep track of where we are in the design matrix
    num_rows <- nrow(data$mask)
    # this is failing on the very last session - index is larger than the number of rows
    # need to figure out why this is happening?
    matrix_range <- curr_matrix_start:(curr_matrix_start + num_rows - 1)
    design_matrix <- design$X.lbm[matrix_range,]
    random_matrix <- design$Z.lbm[matrix_range,]
    curr_matrix_start <- curr_matrix_start + num_rows

    # D differs based on whether we are using homogeneous or inhomogeneous
    if (homogeneous) {
      D <- exp(log_D)/10000
    } else {
      D <- as.vector(exp(design_matrix %*% beta + random_matrix %*% u)/10000)
    } 

    nll <- 0
    nll <- nll - sum(dnorm(u, mean = 0, sd = sigma_u, log = TRUE))

    ## Number of animals detected.
    n <- nrow(data$binary_capture_history)
    ## Number of traps.
    n.traps <- nrow(data$traps)
    ## Number of mask points.
    n.mask <- num_rows
    ## Area of a single mask cell
    a <- data$mask_cell_area

    mask.dists <- crossdist(data$mask[, 1], data$mask[, 2],
                            data$traps[, 1], data$traps[, 2])

    mask.probs <- g0*exp(-mask.dists^2/(2*sigma^2))
    p.avoid <- apply(1 - mask.probs, 1, prod)
    p.det <- 1 - p.avoid

    ## Calculating the effective sampling area.
    esa <- a*sum(p.det)

    # For inhomogeneous, calculate intensity
    if (!homogeneous) {
      fs_denom <- D*p.det
      fs_denom_sum <- sum(fs_denom) * a
    } 

    #Calculating likelihood contribution due to each
    # detected animal's capture history.

    capt.hist <- data$binary_capture_history
    tiny_num <- .Machine$double.xmin

    if (nrow(data$binary_capture_history) != 0) {
      log.f.capt.given.s <- log(mask.probs + tiny_num) %*% t(capt.hist) + log(1-mask.probs) %*% t(1-capt.hist)
      if (!homogeneous) {
        log.f.s <- log(D) + log(p.det + tiny_num) - log(fs_denom_sum) # realised this isn't actually used anywhere... do I still need it?
        log.integrand <- log.f.capt.given.s + log(D) - log(fs_denom_sum)
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
      log.f.n <- dpois(n, D*esa, log = TRUE) 
    } else {
      log.f.n <- dpois(n, fs_denom_sum, log = TRUE) 
    }

    ## Overall log-likelihood. The last part accounts for the fact that we cannot observe an all-zero capture history.
    ll <- log.f.n + log.f.capt
    # Returning negative log-likelihood, or individual capture history probabil ities, depending on capt.prob.
    nll <- nll - ll
    total_ll <- total_ll + nll
  }

  total_ll
}

# Make the above into a closure so that it can be used by MakeADFun

scr_log_likelihood_closure <- function(f, d, h, design) function(p) f(p, d, h, design)