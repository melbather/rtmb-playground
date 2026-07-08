# Test the new consolidated function

# Libraries 
library(spatstat)
library(stringr)
library(RTMB)
library(mgcv)
library(dplyr)

load("data/gibbon-data.RData")
source("fitting-functions/fitting-functions.R")
source("fit_scr.R")
# Source all misc functions
sapply(
  list.files("misc functions", full.names = TRUE, recursive = TRUE), function(i) {
    source(paste0(i))
  })
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
x <- seq(min(fine.mask.df$x), max(fine.mask.df$x), length.out = 100)
y <- seq(min(fine.mask.df$y), max(fine.mask.df$y), length.out = 100)

pred_mask <- expand.grid(x = x, y = y)

plot_density(fit, pred_mask, traps)
