# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(acre)
library(mgcv)
library(ggplot2)
library(dplyr)

# Load functions
source("misc functions/avg_trap_dist.R")
source("get_params.R")
source("scr_log_likelihood.R")
source("misc functions/mask_cell_area.R")
source("fitting-functions/fitting-functions.R")

load("data/gibbon-data.RData")

capt.nobearings <- capt
for (i in 1:length(capt)){
  capt.nobearings[[i]] <- capt[[i]][names(capt[[i]]) != "bearing"]
}

# Format data so my function can use it
gibbons_data <- list()
for (i in 1:length(capt)) {
  gibbons_data[[i]] <- list(
    binary_capture_history = as.data.frame(capt[[i]]$bincapt),
    traps = traps[[i]],
    mask = mask.full[[i]]
  )
}

# Combine all mask dfs into one
mask.df <- cbind(do.call(rbind, mask.full), do.call(rbind, mask.full.df)) |> 
  mutate(forest = ifelse(FOREST_COVER == "DENSE", 1, 0))

# One big design matrix for entire mask
params_and_design_gibbons <- get_params(mask.df, "~s(x, y, k=25)", traps, detfn = "HHN")

# Pre-calculate mask dists
mask_dists <- list()
for (i in 1:length(mask.full)) {
  mask_dists[[i]] <- crossdist(mask.full[[i]][, 1], mask.full[[i]][, 2],
                            traps[[i]][, 1], traps[[i]][, 2])
}

# Test likelihood function - seems to work
scr_log_likelihood(params_and_design_gibbons$params, 
  gibbons_data, 
  FALSE, 
  design = params_and_design_gibbons$design_matrix, 
  mask_dists = mask_dists,
  detfn = "HHN")

# Try with RTMB using closure
obj_scr_gibbons <- RTMB::MakeADFun(
  scr_log_likelihood_closure(
    scr_log_likelihood, 
    gibbons_data, 
    FALSE, 
    params_and_design_gibbons$design_matrix,
    mask_dists,
    "HHN"
  ), 
    params_and_design_gibbons$params, 
    random = "u")
opt_scr_gibbons <- nlminb(obj_scr_gibbons$par, obj_scr_gibbons$fn, obj_scr_gibbons$gr)
sdreport_gibbons <- sdreport(obj_scr_gibbons)
summary(sdreport_gibbons)

# Get X and Z matrices for prediction ------------------------------------------------
pred_df <- data.frame(
  x = seq(min(fine.mask.df$x), max(fine.mask.df$x), length.out = 1000)
)

pred_design <- construct.design(~ s(x, k = 25), 
                                df = pred_df, 
                                orig.df = data.frame(x = mask.df$x), 
                                sm = params_and_design_gibbons$sm, 
                                sm2ran = params_and_design_gibbons$sm2ran)
X_pred <- pred_design$X.lbm
Z_pred <- pred_design$Z.lbm

betas <- summary(sdreport_gibbons) |> 
  as.data.frame() |> 
  filter(grepl("beta", rownames(summary(sdreport_gibbons)))) |> 
  pull(Estimate)

us <- summary(sdreport_gibbons) |> 
  as.data.frame() |> 
  filter(rownames(summary(sdreport_gibbons)) == "u") |> 
  pull(Estimate)

D_pred <- as.vector(exp(X_pred %*% betas + Z_pred %*% us)/10000)

# Plot
plot(pred_df$x, D_pred, type = "l")

# Compare to fit.scr.smooth ----------------------------------------------------------
pred.df <- data.frame(x = 702553.4, y = 1404628.3,
                      FOREST_COVER = "VERY_DENSE",
                      VILLAGE = 5)

start.par <- list(D_betas = log(0.001),
                  link_lambda0 = log(50),
                  link_sigma = log(500))

fit.sm <- fit.scr.smooth(capt.nobearings, traps, mask.full,
                         model = ~ s(x, y, k = 25),
                         mask.df = mask.full.df, pred.df = pred.df,
                         n.occasions = 1, start.par = start.par,
                         tmb.dir = "fitting-functions/tmb", detfn = "HHN")

summary(fit.sm$sdrep)
