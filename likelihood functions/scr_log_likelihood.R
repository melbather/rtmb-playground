# This is where the likelihood function lives now because the main script was getting too crowded.

scr_log_likelihood <- function(params, data_list, homogeneous, design, mask_dists, detfn = "HN") {
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

  # random effects
  sigma_u <- exp(log_sigma_u)
  total_nll <-  -sum(dnorm(u, mean = 0, sd = sigma_u, log = TRUE))
  
  total_ll <- 0
  num_sessions <- length(data_list)
  curr_matrix_start <- 1

  for (i in 1:num_sessions) {
    data <- data_list[[i]]

    # Keep track of where we are in the design matrix
    num_rows <- nrow(data$mask)
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

    ## Number of animals detected.
    n <- nrow(data$binary_capture_history)
    ## Number of traps.
    n.traps <- nrow(data$traps)
    ## Number of mask points.
    n.mask <- num_rows
    ## Area of a single mask cell
    a <- attr(data$mask, "area")

    mask.dists <- mask_dists[[i]]
    

    if (detfn == "HN") {
      mask.probs <- g0*exp(-mask.dists^2/(2*sigma^2))
      p.avoid <- apply(1 - mask.probs, 1, prod)
      p.det <- 1 - p.avoid
      #browser()
      ## Calculating the effective sampling area.
      esa <- a*sum(p.det)
      ADREPORT(esa)

      # For inhomogeneous, calculate intensity
      if (!homogeneous) {
        fs_denom <- D*p.det
        fs_denom_sum <- sum(fs_denom) * a
      } 

    } else if (detfn == "HHN") {
      # Computing mask.probs here is problematic because R sometimes rounds some of these to 1
      lambda <- lambda0*exp(-mask.dists^2/(2*sigma^2))
      ## Calculating the effective sampling area.
      esa <- a*sum(1 - exp(-lambda))
      ADREPORT(esa)

      # For inhomogeneous, calculate intensity
      if (!homogeneous) {
        fs_denom <- D*(1 - exp(-lambda))
        fs_denom_sum <- sum(fs_denom) * a
      } 
    }

    #Calculating likelihood contribution due to each detected animal's capture history.
    capt.hist <- data$binary_capture_history
    tiny_num <- .Machine$double.xmin

    if (nrow(data$binary_capture_history) != 0) {
      if (detfn == "HHN") log.f.capt.given.s <- log(-expm1(-lambda) + tiny_num) %*% t(capt.hist) - lambda %*% t(1-capt.hist)
      else log.f.capt.given.s <- log(mask.probs + tiny_num) %*% t(capt.hist) + log(1-mask.probs) %*% t(1-capt.hist)

      if (!homogeneous) {
        # log.f.s <- log(D) + log(p.det + tiny_num) - log(fs_denom_sum) # realised this isn't actually used anywhere... do I still need it?
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
    # session_ll[[i]] <- ll
    total_ll <- total_ll + ll
  }
  total_nll - total_ll
}

# Make the above into a closure so that it can be used by MakeADFun
scr_log_likelihood_closure <- function(f, d, h, design, m, df) function(p) f(p, d, h, design, m, df)
