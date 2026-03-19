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

# 3. Create basis functions in mgcv and fit unpenalised splines ---------------------------------
library(mgcv)
library(scales)
library(dplyr)

# Create some sample data
set.seed(42)
sample_data_bas <- data.frame(x = sort(runif(n, -1, 1)))
# Using sine to start
sample_data_bas$y <- sin(2 * pi * sample_data_bas$x) + rnorm(n, sd = 0.5)

# These are *penalised* splines! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Try different numbers of knots
m1 <- gam(y ~ s(x, k = 5), data = sample_data_bas, method = "REML")
summary(m1)
m2 <- gam(y ~ s(x, k = 10), data = sample_data_bas, method = "REML")
summary(m2)
m3 <- gam(y ~ s(x, k = 20), data = sample_data_bas, method = "REML")
summary(m3)

# Visualise
plot(sample_data_bas, pch = 19, cex = 0.5, col=alpha("black", 0.5))
lines(sample_data_bas$x, fitted(m1), col = "red", lwd = 2)
lines(sample_data_bas$x, fitted(m2), col = "green", lwd = 2)
lines(sample_data_bas$x, fitted(m3), col = "purple", lwd = 2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now fit *unpenalised* splines ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use gam() to construct a design matrix
# (Code from Ben)
m4 <- gam(y ~ s(x, k = 5), data = sample_data_bas, fit = FALSE)
m4$X

# Create variables basis1, basis2, basis3, basis4, basis5
basis_df <- sample_data_bas |> 
  cbind(as.data.frame(m4$X))
names(basis_df) <- c("x", "y", "intercept", "basis1", "basis2", "basis3", "basis4")

m5 <- lm(y ~ basis1 + basis2 + basis3 + basis4, data = basis_df)
summary(m2)

# See what it looks like - you can see a rough pattern, but it could be more wiggly
plot(sample_data_bas)
lines(sample_data_bas$x, fitted(m5), col = "red", lwd = 2)

# Try more knots 
m6 <- gam(y ~ s(x, k = 10), data = sample_data_bas, fit = FALSE)
m6$X

# Create variables basis1, basis2, basis3, basis4, basis5
basis_df2 <- sample_data_bas |> 
  cbind(as.data.frame(m6$X))
names(basis_df2) <- c("x", "y", "intercept", paste0("basis", 1:9))

m7 <- lm(y ~ basis1 + basis2 + basis3 + basis4 + basis5 + basis6 + basis7 + basis8 + basis9, data = basis_df2)
summary(m7)

# See what it looks like - this looks pretty good!
plot(sample_data_bas)
lines(sample_data_bas$x, fitted(m7), col = "red", lwd = 2)