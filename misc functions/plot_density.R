plot_density <- function(fit, mask) {
  library(ggplot2)

  # Predict density across mask
  pred_df <- data.frame(x = mask$x, y = mask$y) # TODO add covariates from mask 

  pred_design <- construct.design(
    model = fit$model,
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
    filter(rownames(sdreport_summary) == "^u") |> 
    pull(Estimate)

  # Predict density
  D_pred <- as.vector(exp(X_pred %*% betas + Z_pred %*% us)/10000)

  # Make heatmap
  ggplot(pred_df, aes(a, y, fill = D_pred)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(name = "Density") +
    theme_minimal()
}