# Load simulated data
# Using simplified data where half the area is forest and half is not forest
library(spatstat)
library(stringr)
load("simulated_data_simple.RData")

# I think there was a reason why I made some of the covariates strings instead of ints in the simulation
# but I can't remember what it was
# They need to be ints to work in this script
single_session_demo$forest <- as.numeric(single_session_demo$forest)
single_session_demo$protected_areas <- as.numeric(single_session_demo$protected_areas)

# Set up the same as with splines previously
library(RTMB)

# Starting parameters
# For homogeneous case we don't have covariates since density is constant
parameters_scr_homogeneous <- list(
  log_D = log(0.1),
  logit_g0 = qlogis(0.5),
  log_sigma = log(100)
)
# Add covariates to inhomogeneous params
# Only using forest coverage as a covariate for now
parameters_scr_inhomogeneous <- list( 
  logit_g0 = qlogis(0.5),
  log_sigma = log(50),
  beta0 = 0, # intercept term
  # beta1 = 0, # coefficient of x-coordinate
  # beta2 = 0, # coefficient of y-coordinate
  beta3 = 0 # coefficient for forest
)

# Using Ben's function with option to decide if the process is homogeneous or inhomogeneous
# Create global variable for homogeneous/inhomogeneous
# This is not a very slick way to do it, but seems like the easiest way for now 
# because it looks like the likelihood function can only take in one argument when using MakeADFun
homogeneous <- TRUE
#single_session_demo$mask <- acre_model_full$args$mask[[1]]
#single_session_demo$mask_cell_area <- attr(acre_model_full$args$mask[[1]], "area")*10000
scr_likelihood <- function(params) {
  getAll(single_session_demo, params) 
  
  # D differs based on whether we are using homogeneous or inhomogeneous
  if (homogeneous) {
    D <- exp(log_D)/10000
  } else {
    # x <- (single_session_demo$mask[,1] - mean(single_session_demo$mask[,1]))/sd(single_session_demo$mask[,1])
    # y <- (single_session_demo$mask[,2] - mean(single_session_demo$mask[,2]))/sd(single_session_demo$mask[,2])
    # D <-  exp(beta0 + beta1*x + 
    #             beta2*y + 
    #             beta3*single_session_demo$forest)/10000
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

# HOMOGENEOUS 
# homogeneous is set to TRUE above
# Test function
scr_likelihood(parameters_scr_homogeneous)

# Convert likelihood function into object that the optimiser can use 
obj_scr_homogeneous <- MakeADFun(scr_likelihood, parameters_scr_homogeneous)
opt_scr_homogeneous <- nlminb(obj_scr_homogeneous$par, obj_scr_homogeneous$fn, obj_scr_homogeneous$gr)
summary(sdreport(obj_scr_homogeneous)) 


# INHOMOGENEOUS
homogeneous <- FALSE

# Test function again
scr_likelihood(parameters_scr_inhomogeneous)

obj_scr_inhomogeneous <- MakeADFun(scr_likelihood, parameters_scr_inhomogeneous)
opt_scr_inhomogeneous <- nlminb(obj_scr_inhomogeneous$par, obj_scr_inhomogeneous$fn, obj_scr_inhomogeneous$gr)
inhomogeneous_summary <- summary(sdreport(obj_scr_inhomogeneous))


# Compare to results using acre
library(acre)

#(Using MSc code)

# Create covariates data frame --------------------------------------------------
cov_df <- data.frame(x = single_session_demo$mask$x,
                     y = single_session_demo$mask$y,
                     forest = as.vector(t(single_session_demo$forest)))

single_session_demo$long_form_capture_history <- single_session_demo$long_form_capture_history[,-5]
# Create an acre object ---------------------------------------------------------
acre_object <- read.acre(single_session_demo$long_form_capture_history, single_session_demo$traps, 
                          control.mask = list(buffer = 85*5),
                          loc.cov = cov_df, 
                          dist.cov = list(villages = village_locations1))

#Create a full model  ----------------------------------------------------------
acre_model_forest <- fit.acre(acre_object, 
                          model = list(D=~forest))

#Summary output ----------------------------------------------------------
summary(acre_model_forest)


# Compare denisty between acre and my code above
inhomogeneous_summary |> 
  as.data.frame() |> 
  filter(str_detect(row.names(inhomogeneous_summary), "D")) |> 
  pull(Estimate) |> 
  unique()

predict(acre_model_forest, newdata = data.frame(forest = c(1, 0)))
