# Function that calls all other functions!
# Full function that creates the design matrix, sets parameter start values, 
# fits the model using RTMB, and prints out a summary

# bin_capt: A list of binary capture histories, where each element corresponds to one session.
# mask_coords: A list containing the coordinates of mask cells, where each element 
# in the list corresponds to one session.
# mask_df: A data frame containing all mask points across all sessions 
# (this can include covariate information in addition to coordinates).
# traps: A list containing the coordinates of traps, where each element in the list
# corresponds to one session.
# formula: A string containing the formula to be used in the model
# detfn: The detection function. Either HN or HHN. Default is HN.

# if num_species > 1
# bin_capt should be in the following format:
# bin_capt <- list(
#   session_1 = list(
#     species_1 = species_1_capt_session_1,
#     species_2 = species_2_capt_session_1
#   ),
#   session_2 = list(
#     species_1 = species_1_capt_session_2,
#     species_2 = species_2_capt_session_2
#   )
# )

fit_scr <- function(
  bin_capt, 
  mask_coords, 
  mask_df, 
  traps, 
  formula, 
  num_species = 1,
  num_fields = NULL,
  detfn = "HN", 
  sigma_upper_limit = NULL) {
  # Load functions
  source("misc functions/avg_trap_dist.R")
  source("misc functions/get_params.R")
  source("likelihood functions/scr_log_likelihood.R")
  source("likelihood functions/scr_log_likelihood_multi.R")
  source("misc functions/mask_cell_area.R")

  data <- list()
  # Consolidate data
  if (num_species == 1) {   
    for (i in 1:length(bin_capt)) { 
      data[[i]] <- list(
        binary_capture_history = as.data.frame(bin_capt[[i]]),
        traps = traps[[i]],
        mask = mask_coords[[i]]
      )
    }
  } else {
    for (i in 1:length(bin_capt)) { 
      data[[i]] <- list()
      for (j in 1:length(bin_capt[[i]])) {
        data[[i]][[j]] <- list(
          binary_capture_history = as.data.frame(bin_capt[[i]][[j]]),
          traps = traps[[i]],
          mask = mask_coords[[i]]
        )
      }
    }
  }

  # Calculate masks dists
  mask_dists <- list()
  for (i in 1:length(mask_coords)) {
    mask_dists[[i]] <- crossdist(mask_coords[[i]][, 1], mask_coords[[i]][, 2],
                              traps[[i]][, 1], traps[[i]][, 2])
  }

  # Get starting params and design matrix
  params_and_design <- get_params(mask_df, formula, traps, num_species, num_fields, detfn)

  # Fit the model using likelihood function closure and RTMB
  # TODO get rid of this if/else block and just use one likelihood function
  if (num_species == 1) {
    obj_scr <- RTMB::MakeADFun(
    scr_log_likelihood_closure(
      scr_log_likelihood, 
      data, 
      FALSE, 
      params_and_design$design_matrix,
      mask_dists,
      detfn
    ), 
    params_and_design$params, 
    random = "u")
  } else {
    obj_scr <- RTMB::MakeADFun(
      scr_log_likelihood_closure_multi(
        scr_log_likelihood_multi, 
        data, 
        FALSE, 
        params_and_design$design_matrix,
        mask_dists,
        detfn
      ), 
    params_and_design$params, 
    random = "u")
  }

  if (!is.null(sigma_upper_limit)) {
    opt_scr <- nlminb(obj_scr$par, obj_scr$fn, obj_scr$gr, 
      upper = c(Inf, log(sigma_upper_limit), Inf, Inf, Inf, Inf))
  } else {
    opt_scr <- nlminb(obj_scr$par, obj_scr$fn, obj_scr$gr)
  }
  sdreport <- sdreport(obj_scr)

  # Return model, starting params, design matrix, opt_scr, and sdreport
  list(
    fit = obj_scr,
    start_pars = params_and_design$params,
    design_matrix = params_and_design$design_matrix,
    sm = params_and_design$sm,
    sm2ran = params_and_design$sm2ran,
    opt_scr = opt_scr,
    sdreport = sdreport,
    orig_mask = mask_df,
    model = formula,
    capture_hist = bin_capt
  )

}
