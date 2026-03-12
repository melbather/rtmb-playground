# Learning to use RTMB!
# Reference: https://cran.r-project.org/web/packages/RTMB/vignettes/RTMB-introduction.html
library(RTMB)

# 1. LINEAR REGRESSION ---------------------------------------------------------------------
# Simulate some data to use
set.seed(42)

n <- 1000
x_lin <- runif(n, -1, 1)

beta0_lin <- 1
beta1_lin <- 2
sigma_lin <- 3

y_lin <- beta0_lin + beta1_lin * x_lin + rnorm(n, 0, sigma_lin)

sample_data_lin <- list(x = x_lin, y = y_lin)

# Negative log likelihood function
linreg <- function(params) {
  # From ref above: "The getAll function makes all the list elements 
  # of data and parameters visible inside the function, so that one can write 
  # e.g. weight rather than ChickWeight$weight"

  # Q FOR BEN: ref says OBS() function is optional here and "enables extra RTMB features"
  # and it is "needed to enable automatic simulation and residual calculations from the model object".
  # Not sure I 100% follow this

  getAll(sample_data_lin, params)
  mu <- beta0 + beta1 * x_lin
  nll <- -sum(dnorm(y_lin, mu, sigma, log = TRUE))
  nll
}

# Param starting vals 
# (set initial vals for the optimiser)
parameters_lin <- list(
  beta0 = 0,
  beta1 = 0,
  sigma = 1
)

# Convert likelihood function into object that the optimiser can use 
obj_lin <- MakeADFun(linreg, parameters_lin)

# Fit the model
# Q FOR BEN: would like to know a bit more about what this function is actually doing but not sure how important this is
opt_lin <- nlminb(obj_lin$par, obj_lin$fn, obj_lin$gr)

# Get estimates + std errors - pretty accurate!
sdreport(obj_lin)

# 2. POLYNOMIAL REGRESSION ------------------------------------------------------------------
# Simulate some data to use
set.seed(42)

x_poly <- runif(n, -1, 1)

beta0_poly <- 1
beta1_poly <- 2
beta2_poly <- 3
sigma_poly <- 3

y_poly <- beta0_poly + beta1_poly * x_poly + beta2_poly * x_poly^2 + rnorm(n, 0, sigma_poly)

sample_data_poly <- list(x = x_poly, y = y_poly)

# Negative log likelihood function
polyreg <- function(params) {
  getAll(sample_data_poly, params)
  mu <- beta0 + beta1 * x_poly + beta2 * x_poly^2
  nll <- -sum(dnorm(y, mu, sigma, log = TRUE))
  nll
}

# Param starting vals 
# (set initial vals for the optimiser)
parameters_poly <- list(
  beta0 = 0,
  beta1 = 0,
  beta2 = 0,
  sigma = 1
)

# Convert likelihood function into object that the optimiser can use 
obj_poly <- MakeADFun(polyreg, parameters_poly)

# Fit the model
opt_poly <- nlminb(obj_poly$par, obj_poly$fn, obj_poly$gr)

# Get estimates + std errors - pretty accurate!
sdreport(obj_poly)
