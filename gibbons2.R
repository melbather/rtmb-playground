# Test the new consolidated function

# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(mgcv)
library(dplyr)

load("data/gibbon-data.RData")
source("fit_scr.R")
source("misc functions/plot_density.R")

# Function needs a list of binary capt histories only
bin_capt <- lapply(capt, function(x) x[["bincapt"]])

# Combine all mask dfs into one
mask.df <- cbind(do.call(rbind, mask.full), do.call(rbind, mask.full.df)) |> 
  mutate(forest = ifelse(FOREST_COVER == "DENSE", 1, 0))

fit <- fit_scr(
  bin_capt, 
  mask.full, 
  mask.df, 
  traps, 
  "~s(x, y, k=25)",
  "HHN"
)

summary(fit$sdreport)

# Try density plotting function
pred_mask <- data.frame(
  x = seq(min(fine.mask.df$x), max(fine.mask.df$x), length.out = 1000),
  y = seq(min(fine.mask.df$y), max(fine.mask.df$y), length.out = 1000)
)

plot_density(fit, pred_mask)
