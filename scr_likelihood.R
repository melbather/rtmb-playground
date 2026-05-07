# This is where the likelihood function lives now because the main script was getting too crowded.

scr_likelihood <- function(params) {
  getAll(single_session_demo, params) 
  
  # D differs based on whether we are using homogeneous or inhomogeneous
  if (homogeneous) {
    D <- exp(log_D)/10000
  } else {
    # For now, make density ONLY depend on forest coverage
    D <- exp(beta0 + beta3*single_session_demo$forest)/10000
  } 
  # ADREPORT(D)
  g0 <- plogis(logit_g0)
  sigma <- exp(log_sigma)

  ## Number of animals detected.
  n <- nrow(single_session_demo$binary_capture_history)
  ## Number of traps.
  n.traps <- nrow(single_session_demo$traps)
  ## Number of mask points.
  n.mask <- nrow(single_session_demo$mask)
  ## Area of a single mask cell
  a <- single_session_demo$mask_cell_area/10000

  ## Constructing a distance matrix. The element (i, j) gives the
  ## distance between the ith mask point and the jth trap. A better
  ## implementation would involve computing these distances once,
  ## outside this function, and then passing them in as an argument,
  ## to avoid recomputing the distances multiple times during model
  ## fitting.
  mask.dists <- crossdist(single_session_demo$mask[, 1], single_session_demo$mask[, 2],
                            single_session_demo$traps[, 1], single_session_demo$traps[, 2])
  ## Constructing a detection probability matrix. The element (i, j)
  ## gives the probability of an animal located at the ith mask
  ## point being detected at the jth trap.
  mask.probs <- g0*exp(-mask.dists^2/(2*sigma^2))
  ## Constructing a detection probability vector. The ith element
  ## gives the probability of an animal located at the ith mask
  ## point being detected by *at least one* trap.
  p.avoid <- apply(1 - mask.probs, 1, prod)
  p.det <- 1 - p.avoid
  ## Calculating the effective sampling area.
  esa <- a*sum(p.det)
  ADREPORT(esa)
  # ADREPORT(D)
  ADREPORT(g0)
  ADREPORT(sigma)
  # For inhomogeneous, calculate intensity
  # Unsure if this is correct!
  if (!homogeneous) {
    fs_denom <- D*p.det
    fs_denom_sum <- sum(fs_denom) * a
  } 

  #Calculating likelihood contribution due to each
  # detected animal's capture history.

  capt.hist <- single_session_demo$binary_capture_history
  tiny_num <- .Machine$double.xmin
  log.f.capt.given.s <- log(mask.probs + tiny_num) %*% t(capt.hist) + log(1-mask.probs) %*% t(1-capt.hist)
  if (!homogeneous) {
    log.f.s <- log(D) + log(p.det + tiny_num) - log(fs_denom_sum)
    log.integrand <- log.f.capt.given.s + log(D) - log(fs_denom_sum)
  } else {
    log.integrand <- log.f.capt.given.s - log(esa)
  }
  f.capt <- colSums(exp(log.integrand) * a)

  # ## Log-likelihood contribution from all capture histories
  # ## calculated by the log of the sum of the individual likelihood
  # ## contributions.
  log.f.capt <- sum(log(f.capt + tiny_num))
  
  # ## Log-likelihood contribution from the number of animals
  # ## detected.
  if (homogeneous) {
    log.f.n <- dpois(n, D*esa, log = TRUE) 
  } else {
    log.f.n <- dpois(n, fs_denom_sum, log = TRUE) 
  }

  # ## Overall log-likelihood. The last part accounts for the fact
  # ## that we cannot observe an all-zero capture history.
  ll <- log.f.n + log.f.capt
  # ## Returning negative log-likelihood, or individual capture
  # ## history probabilities, depending on capt.prob.
  -ll
}