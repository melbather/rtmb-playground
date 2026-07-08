plot_density <- function(fit, mask, detectors) {
  library(ggplot2)

  # Predict density across mask
  pred_df <- data.frame(x = mask$x, y = mask$y) # TODO add covariates from mask 

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

  #browser()

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

  # Combine all detectors together to plot on top
  all_detectors <- bind_rows(
    lapply(detectors, function(mat) {
      data.frame(
        x = mat[, 1],
        y = mat[, 2]
      )
    })
  )

  # Make heatmap
  ggplot(pred_df, aes(x, y, fill = D_pred)) +
    geom_raster() +
    coord_equal() +
    geom_point(data = all_detectors, 
      aes(x = x, y = y), colour = "red",
      inherit.aes = FALSE) +
    scale_fill_viridis_c(name = "Density") +
    theme_minimal()
}