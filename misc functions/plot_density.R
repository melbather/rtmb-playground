plot_density <- function(
  fit, mask, 
  detectors = NULL, 
  plot_det_density = FALSE, 
  animal_coords = NULL,
  title = NULL
) {
  
  library(ggplot2)

  # Predict density across mask
  pred_df <- data.frame(x = mask$x, y = mask$y)

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

  # Make heatmap
  p <- ggplot(pred_df, aes(x, y, fill = D_pred)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(name = "Density") +
    theme_minimal()

  if (!is.null(detectors)) {
      # Combine all detectors together to plot on top
      all_detectors <- bind_rows(
        lapply(detectors, function(mat) {
          data.frame(
            x = mat[, 1],
            y = mat[, 2]
          )
        })
      )
    
    if (plot_det_density) {
      # assuming the same detectors across all sessions (or looking only at the first session)
      num_captures <- colSums(fit$capture_hist[[1]])
      det_and_capt <- data.frame(
        x = detectors[[1]][,1], 
        y = detectors[[1]][,2],
        num_captures = num_captures)
      p <- p +
        geom_point(
          data = det_and_capt,
          aes(x = x, y = y, size = num_captures),
          alpha = 0.6,
          inherit.aes = FALSE
        ) +
        labs(size = "Number of detections")
    } else {
      p <- p +
        geom_point(
          data = all_detectors, 
          aes(x = x, y = y), 
          colour = "black",
          inherit.aes = FALSE
        )
    }
    
  }

  if (!is.null(animal_coords)) {
    animal_coords <- as.data.frame(animal_coords)
    colnames(animal_coords) <- c("x", "y")
    animal_coords$point_type <- "Activity centre"
    p <- p + 
      geom_point(
        data = animal_coords,
        aes(x = x, y = y, colour = point_type),
        alpha = 0.8,
        inherit.aes = FALSE
    ) +
      labs(colour = NULL)
  }

  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  p

}