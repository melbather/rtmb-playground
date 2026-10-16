# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(mgcv)
library(dplyr)
library(fields)

# Get model functions to work using the data from sim_multi_species.R
load("sim data new/multi_species_sim_data.RData")

source("fitting-functions/fitting-functions.R")
source("fit_scr.R")
# Source all misc functions
sapply(
  list.files("misc functions", full.names = TRUE, recursive = TRUE), 
  function(i) {
    source(paste0(i))
  })

# First, need to put data into format that fit_scr needs
# only one session for this
bin_capt <- list(
  session1 <- list(
    species1 = random_capture_hist_no_zero1,
    species2 = random_capture_hist_no_zero2
  )
)

fit_output <- fit_scr(
  bin_capt, 
  list(mask), 
  mask, 
  list(detectors), 
  formula = "~s(x, y, k=25)",
  num_species = 2, 
  num_fields = 2
)

# Testing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
bin_capt_species1 <- list(
  session1 = list(
    species1 = random_capture_hist_no_zero1
  )
)
bin_capt_species2 <- list(
  session1 = list(
    species1 = random_capture_hist_no_zero2
  )
)
# 1. fit two separate single species models
fit_species1 <- fit_scr(
  bin_capt_species1,
  list(mask),
  mask,
  list(detectors),
  formula = "~s(x, y, k=25)",
  num_species = 1,
  num_fields = 1
)

fit_species2 <- fit_scr(
  bin_capt_species2,
  list(mask),
  mask,
  list(detectors),
  formula = "~s(x, y, k=25)",
  num_species = 1,
  num_fields = 1
)

# 2. multispecies model with diagonal elements in the loading matrix fixed at 1
fit_output_multi_fixed1s <- fit_scr(
  bin_capt, 
  list(mask), 
  mask, 
  list(detectors), 
  formula = "~s(x, y, k=25)",
  num_species = 2, 
  num_fields = 2
)

#save.image("multi_models.RDS")
load("multi_models.RDS")

# 3.multispecies model with the diagonal elements estimated
# (using a second version of the fitting function for this)
source("fit_scr v2.R")

fit_output_multi_est_diag <- fit_scr2(
  bin_capt, 
  list(mask), 
  mask, 
  list(detectors), 
  formula = "~s(x, y, k=25)",
  num_species = 2, 
  num_fields = 2
)

# create df with all animal coords
all_animal_coords <- rbind(
  data.frame(species1$animal_coords, species = "Species 1"),
  data.frame(species2$animal_coords, species = "Species 2")
)

# plot: each estimated wiggly surface, both estimates species' wiggly surface
plot_density(fit_species1, mask, list(detectors), title = "Species 1")
plot_density(fit_species2, mask, list(detectors), title = "Species 2")
plot_density(
  fit_output_multi_fixed1s, 
  mask, 
  list(detectors), 
  title = "Both species",
  num_species = 2,
  num_fields = 2,
  animal_coords = list(all_animal_coords)
)

# estimate sigma_u + diag loading
fit_output_multi_est_diag_loading <- fit_scr3(
  bin_capt, 
  list(mask), 
  mask, 
  list(detectors), 
  formula = "~s(x, y, k=25)",
  num_species = 2, 
  num_fields = 2
)

# plot % change of single species to multi species for each pixel
# how to do this?
# thinking to calculate the % diff in predicted density for each mask cell
# then plot that?

# get % diff for in pred density for each mask cell
# function to get predicted density from the single species models
get_pred_D <- function(mask, species_num, fit) {
  pred_df <- data.frame(x = mask$x, y = mask$y, species = paste0("Species", species_num)) # I actually don't think the species label is necessary
  pred_design <- construct.design(
      model = as.formula(fit$model),
      df = pred_df,
      sm = fit$design$sm,
      sm2ran = fit$design$sm2ran,
      orig.df = data.frame(
        x = fit$orig_mask$x, 
        y = fit$orig_mask$y
      )
    )
    X_pred <- pred_design$X.lbm
    Z_pred <- pred_design$Z.lbm

    sdreport_summary <- as.data.frame(summary(fit$sdreport))

    # Pull estimates for betas and us
    betas <- sdreport_summary |> 
      filter(grepl("^beta", rownames(sdreport_summary))) |> 
      pull(Estimate)

    us <- sdreport_summary |> 
      filter(grepl("^u", rownames(sdreport_summary))) |> 
      pull(Estimate)

    # Predict density
    D_pred <- as.vector(exp(X_pred %*% betas + Z_pred %*% us)/10000)
}
# get predicted densities for each species
pred_D_species1 <- get_pred_D(mask, 1, fit_species1)
pred_D_species2 <- get_pred_D(mask, 2, fit_species2)

# compare maps for fixed 1s on diag and not fixed

# check numbner of betas - change names - output of construct.design
