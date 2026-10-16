plot_density <- function(
  fit, mask, 
  detectors = NULL, 
  plot_det_density = FALSE, 
  animal_coords = NULL,
  title = NULL,
  num_species = 1,
  num_fields = 1
) {
  
  library(ggplot2)

  pred_df <- data.frame(x = mask$x, y = mask$y)

  if (num_species == 1) {
    # Predict density across mask
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
  } else {
    # Predict density across mask
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

    # Pull estimates for betas, us, and loading matrix
    betas <- sdreport_summary |> 
      filter(grepl("^beta", rownames(sdreport_summary))) |> 
      pull(Estimate)

    us <- sdreport_summary |> 
      filter(grepl("^u", rownames(sdreport_summary))) |> 
      pull(Estimate)

    loading_values <- sdreport_summary |> 
      filter(grepl("^loading", rownames(sdreport_summary))) |> 
      pull(Estimate)
    
    betas <- matrix(betas, ncol = num_species)
    us <- matrix(us, nrow = ncol(Z_pred), ncol = num_fields)
    loading_matrix <- matrix(0, nrow = num_species, ncol = num_fields)
    diag(loading_matrix) <- 1
    loading_index <- 1
    for (k in 1:num_fields) {
      if (k < num_species) {
        for (s in (k + 1):num_species) {
          loading_matrix[s, k] <- loading_values[loading_index]
          loading_index <- loading_index + 1
        }
      }
    }

    # Predict density
    fixed_effects <- X_pred[, 1:nrow(betas)] %*% betas
    wiggly_fields <- Z_pred %*% us
    spatial_effects <- wiggly_fields %*% loading_matrix
    D_pred <- exp(fixed_effects + spatial_effects) / 10000

    pred_df <- bind_rows(
      lapply(1:num_species, function(s) {
        data.frame(
          x = mask$x,
          y = mask$y,
          D_pred = D_pred[, s],
          species = paste("Species", s)
        )
      })
    )

    # Make heatmap
    p <- ggplot(pred_df, aes(x, y, fill = D_pred)) +
      geom_raster() +
      coord_equal() +
      scale_fill_viridis_c(name = "Density") +
      facet_wrap(~species) +
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
          facet_wrap(~species) +
          labs(size = "Number of detections")
      } else {
        p <- p +
          geom_point(
            data = all_detectors, 
            aes(x = x, y = y), 
            colour = "black",
            inherit.aes = FALSE
          ) +
          facet_wrap(~species)
      }
      
    }

    if (!is.null(animal_coords)) {
      animal_coords <- as.data.frame(animal_coords)
      colnames(animal_coords) <- c("x", "y", "species")
      animal_coords$point_type <- "Activity centre"
      p <- p + 
        geom_point(
          data = animal_coords,
          aes(x = x, y = y, colour = point_type),
          alpha = 0.8,
          size = 0.5,
          inherit.aes = FALSE
      ) +
        facet_wrap(~species) +
        labs(colour = NULL)
    }
  }

  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  p

}