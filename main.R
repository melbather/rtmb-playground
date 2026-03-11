# Learning to use RTMB!
# Reference: https://cran.r-project.org/web/packages/RTMB/vignettes/RTMB-introduction.html
library(RTMB)

# 1. LINEAR REGRESSION ---------------------------------------------------------------------
# Simulate some data to use
set.seed(42)

n <- 1000
x <- runif(n, -1, 1)

beta0 <- 1
beta1 <- 2
sigma <- 3

y <- beta0 + beta1 * x + rnorm(n, 0, sigma)

sample_data <- list(x = x, y = y)

# Negative log likelihood function
linreg <- function(params) {
  # From ref above: "The getAll function makes all the list elements 
  # of data and parameters visible inside the function, so that one can write 
  # e.g. weight rather than ChickWeight$weight"

  # Q FOR BEN: ref says OBS() function is optional here and "enables extra RTMB features"
  # and it is "needed to enable automatic simulation and residual calculations from the model object".
  # Not sure I 100% follow this

  getAll(sample_data, params)
  mu <- beta0 + beta1 * x
  nll <- -sum(dnorm(y, mu, sigma, log = TRUE))
  nll
}

# Param starting vals 
# (set initial vals for the optimiser)
parameters <- list(
  beta0 = 0,
  beta1 = 0,
  sigma = 1
)

# Convert likelihood function into object that the optimiser can use 
obj <- MakeADFun(linreg, parameters)

# Fit the model
# Q FOR BEN: would like to know a bit more about what this function is actually doing but not sure how important this is
opt <- nlminb(obj$par, obj$fn, obj$gr)

# Get estimates + std errors - pretty accurate!
sdr <- sdreport(obj)
sdr

# 2. POLYNOMIAL REGRESSION ------------------------------------------------------------------
# TODO