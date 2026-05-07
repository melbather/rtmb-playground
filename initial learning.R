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
sample_data_bas <- data.frame(x = sort(runif(100, -1, 1)))
# Using sine to start
sample_data_bas$y <- sin(2 * pi * sample_data_bas$x) + rnorm(100, sd = 0.5)

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
xx <- seq(0, 1, length.out = 1000) # figure out what the basis function values are for these values then yy <- predict(model, newdata = [new basis function values in here])
plot(sample_data_bas)
lines(sample_data_bas$x, fitted(m7), col = "red", lwd = 2)


# 4. LINEAR MIXED EFFECTS MODELS ----------------------------------------------------------------
library(lme4)
set.seed(42)

# Simulate data (clusters of animals)
# Ref: https://bookdown.org/ronsarafian/IntrotoDS/lme.html
num_groups <- 10
n_per_group <- 15
N <- num_groups * n_per_group # total animals
groups <- rep(1:num_groups, each = n_per_group) |> as.factor()
x_me <- runif(N, -1, 1)

beta0_me <- 1
beta1_me <- 2

# Random group effects
sigma_u <- 3
sigma_fixed <- 1
u <- rnorm(num_groups, 0, sigma_u)

y_me <- beta0_me + beta1_me * x_me + u[as.numeric(groups)] + rnorm(N, 0, sigma_fixed)

data_mixed_effects <- list(
  y = y_me,
  x = x_me,
  group = groups,
  J = num_groups
)

# Negative log likelihood function
# Ref: https://cran.r-project.org/web/packages/RTMB/vignettes/RTMB-introduction.html
me_likelihood <- function(params) {
  getAll(data_mixed_effects, params)
  mu <- beta0 + beta1 * x + u[as.numeric(groups)]
  nll <- 0
  nll <- nll - sum(dnorm(u, mean = 0, sd = sigma_u, log = TRUE))
  nll <- nll - sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  nll
}


# Param starting vals 
# (set initial vals for the optimiser)
parameters_me <- list(
  beta0 = 0,
  beta1 = 0,
  sigma_u = 1,
  sigma = 1,
  u = rep(0, num_groups)
)

# Make objective function
obj_me <- MakeADFun(me_likelihood, parameters_me, random = "u")

# Fit the model
opt_me <- nlminb(obj_me$par, obj_me$fn, obj_me$gr)

# Get estimates + std errors - closeish?
sdreport(obj_me)

# Compare to model using lme4
lme_fit <- lmer(y ~ x + (1 | group), data_mixed_effects, REML = FALSE)
summary(lme_fit)

# (From meeting w Ben)
sm <- m4$smooth[[1]]
sm$X <- m4$X[, 2:5]
sm2ran <- smooth2random(sm, "", type = 2)

# 5. Fit unpenalised splines above using penalised splines in RTMB -------------------------------
# Use Ben's function construct.design()
# Using sample_data_bas

source("construct-design.R")

# Construct design matrix
design <- construct.design(~ s(x, k = 10), df = sample_data_bas)

# Fixed-effects design matrix
X.lbm <- design$X.lbm
# Random-effects design matrix
Z.lbm <- design$Z.lbm

parameters_ps <- list(
  beta0 = rep(0, ncol(X.lbm)), 
  u = rep(0, ncol(Z.lbm)),  
  log_sigma_u = 1,
  log_sigma = 1
)

ps_likelihood <- function(params) {
  getAll(sample_data_bas, params)
  sigma <- exp(log_sigma)
  sigma_u <- exp(log_sigma_u)
  ADREPORT(sigma)
  ADREPORT(sigma_u)
  mu <- as.vector(X.lbm %*% beta0 + Z.lbm %*% u)
  ADREPORT(mu)

  nll <- 0
  nll <- nll - sum(dnorm(u, mean = 0, sd = sigma_u, log = TRUE))
  nll <- nll - sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  nll
}

obj_ps <- MakeADFun(ps_likelihood, parameters_ps, random = "u")
opt_ps <- nlminb(obj_ps$par, obj_ps$fn, obj_ps$gr) 

summary(sdreport(obj_ps))

# Compare to mgcv
m_mgcv <- gam(y ~ s(x, k = 10), data = sample_data_bas, method = "ML")
mu_gam <- fitted(m_mgcv)

fit_rtmb <- obj_ps$env$parList(obj_ps$env$last.par.best)
mu_rtmb <- as.vector(X.lbm %*% fit_rtmb$beta0 + Z.lbm %*% fit_rtmb$u)

cbind(
  x = sample_data_bas$x,
  y = sample_data_bas$y,
  mu_rtmb = mu_rtmb,
  mu_gam = mu_gam
)

plot(sample_data_bas$x, sample_data_bas$y, pch = 16)

ord <- order(sample_data_bas$x)

lines(sample_data_bas$x[ord], mu_rtmb[ord], lwd = 3, col = "blue")
lines(sample_data_bas$x[ord], mu_gam[ord], lwd = 2, col = "red")
legend("topright", legend = c("RTMB", "mgcv"), lwd = c(3, 2), col = c("blue", "red"))
