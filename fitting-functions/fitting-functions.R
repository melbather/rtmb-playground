library(mgcv)
library(TMB)
library(fields)


convert.traps <- function(traps){
  n.traps <- nrow(traps)
  colnames(traps) <- c("x", "y")
  traps.df <- data.frame(names = 1:n.traps, traps)
  read.traps(data = traps.df, detector = "proximity")
}

calc.dists <- function(points1, points2){
  apply(points1, 1, function(x) sqrt((x[1] - points2[, 1])^2 + (x[2] - points2[, 2])^2))
}

calc.bearings <- function(points1, points2){
  x.diff <- apply(points2, 1, function(x) x[1] - points1[, 1])
  y.diff <- apply(points2, 1, function(x) x[2] - points1[, 2])
  out <- atan(x.diff/y.diff)
  out[y.diff < 0] <- out[y.diff < 0] + pi
  out[y.diff >= 0 & x.diff < 0] <- out[y.diff >= 0 & x.diff < 0] + 2*pi
  out
}

image_xyz <- function(x, y, z, ...){
  u.x <- sort(unique(x))
  u.y <- sort(unique(y))
  z.mat <- squarify(cbind(x, y), z)
  image(u.x, u.y, z.mat, ...)
}

s2rPred <- function(sm, re, data) {
  ## Function to aid prediction from smooths represented as type==2
  ## random effects. re must be the result of smooth2random(sm,...,type=2).
  X <- PredictMat(sm,data)   ## get prediction matrix for new data
  ## transform to r.e. parameterization
  if (!is.null(re$trans.U)) X <- X%*%re$trans.U
  X <- t(t(X)*re$trans.D)
  ## re-order columns according to random effect re-ordering...
  X[,re$rind] <- X[,re$pen.ind!=0]
  ## re-order penalization index in same way
  pen.ind <- re$pen.ind; pen.ind[re$rind] <- pen.ind[pen.ind>0]
  ## start return object...
  r <- list(rand=list(),Xf=X[,which(re$pen.ind==0),drop=FALSE])
  for (i in 1:length(re$rand)) { ## loop over random effect matrices
    r$rand[[i]] <- X[,which(pen.ind==i),drop=FALSE]
    attr(r$rand[[i]],"s.label") <- attr(re$rand[[i]],"s.label")
  }
  names(r$rand) <- names(re$rand)
  r
}

## This is broken.
predict.effect <- function(fit, which.smooth, df){## Getting estimated coefficients for the fixed effects.
  summary.fixed <- summary(fit$sdrep, "fixed")
  summary.betas <- summary.fixed[rownames(summary.fixed) == "D_betas", , drop = FALSE]
  ## Total number of betas.
  n.beta <- nrow(summary.betas)
  ## Number of unpenalised basis functions for each smooth effect.
  n.unpen <- sapply(fit$sm2ran, function(x) ncol(x$Xf))
  ## Extracting the coefficients for the unpenalised basis
  ## functions.
  summary.betas <- summary.betas[(n.beta - sum(n.unpen) + 1):n.beta, , drop = FALSE]
  n.smooth <- length(fit$sm2ran)
  beta.fac <- rep(1:n.smooth, times = n.unpen)
  summary.betas <- summary.betas[beta.fac == which.smooth, , drop = FALSE]
  ## Getting estimated coefficients for the penalised basis
  ## functions.
  summary.random <- summary(fit$sdrep, "random")[fit$u.fac %in% which.smooth, , drop = FALSE]
  ## Extracting the smooth objects.
  sm <- fit$sm[[which.smooth]]
  sm2ran <- fit$sm2ran[[which.smooth]]
  ## Creating new design matrices.
  r <- s2rPred(sm, sm2ran, data = df)
  ## Generating predicted effect.
  r$Xf %*% summary.betas[, 1] + r$rand$Xr %*% summary.random[, 1]
}


                                        # summary = TRUE - returns summary stats mean, sd, q0.025, q0.975
                                        # summary = FALSE - returns the n realisations of the density random field
predict.D <- function(fit, df, uncertainty = FALSE, summary = TRUE, n = 100){
  design.mats <- construct.design(model = fit$args$model,
                                  sm = fit$sm, sm2ran = fit$sm2ran, df = df,
                                  orig.df = fit$df, unsuitable = fit$args$unsuitable)
  summary.fixed <- summary(fit$sdrep, "fixed")
  summary.betas <- summary.fixed[rownames(summary.fixed) == "D_betas", ]
  summary.random <- summary(fit$sdrep, "random")
  pred.D <- exp(design.mats$X.lbm %*% summary.betas[, 1] + design.mats$Z.lbm %*%
                summary.random[, 1])
  pred.D[is.na(pred.D)] <- 0
  
  if (uncertainty){
    library(INLA)
    Q = fit$sdrep$jointPrecision 
    samps = INLA::inla.qsample(n = n, Q)
    rownames(samps) = rownames(Q)
    betas = samps[rownames(samps) == "D_betas",]
    us = samps[rownames(samps) == "u",]
    pred.Ds = exp(design.mats$X.lbm %*% (summary.betas[,1] + betas) + design.mats$Z.lbm %*% (summary.random[,1] + us))
    if (summary){
      out = data.frame(mean = pred.D,
                       sd = apply(pred.Ds, 1, sd),
                       q0.025 = apply(pred.Ds, 1, function(x) quantile(x, prob = 0.025)),
                       q0.975 = apply(pred.Ds, 1, function(x) quantile(x, prob = 0.975))
                       )
    } else{
      out = pred.Ds
    }
  } else{
    out = pred.D
  }
  
  return(out)
}

## Constructing design matrices for model fitting and prediction.
##
## model:      A formula for a model.
## df:         A data frame from which to construct design matrices.
## sm, s2ran:  If design matrices are to be constructed from new data,
##             but based on an already fitted model, then these objects
##             must be provided.
## orig.df:    If we are creating design matrices for new data based on
##             an already fitted model, then we also need to include
##             the original data frame. The only reason for this is
##             because we need to know the possible levels for any
##             categorial variables.
## unsuitable: A list specifying unsuitable habitat. Same as
##             fit.smooth.scr().
construct.design <- function(model, df, sm = NULL, sm2ran = NULL, orig.df = NULL, unsuitable = NULL){
  ## Indicator for whether or not we need to build the sm and sm2ran
  ## objects.
  build.sm <- is.null(sm)
  ## Setting unsuitable habitat to NA.
  if (!is.null(unsuitable)){
    n.unsuitable <- length(unsuitable)
    for (i in 1:n.unsuitable){
      df[names(unsuitable)[i]][df[names(unsuitable)[i]] == unsuitable[[i]]] <- NA
    }
  }
  ## Full model formula with response.
  model <- update(model, dummy.response ~ .)
  ## Adding new data frame to the original data frame.
  if (!build.sm){
    full.df <- rbind(orig.df, df[colnames(orig.df)])
  } else {
    full.df <- df
  }
  ## Indicator for which rows have fully observed data.
  full.obs.rows <- apply(full.df, 1, function(x) !any(is.na(x)))
  ## Creating dummy response.
  full.df$dummy.response <- 1:nrow(full.df)
  ## GAM object.
  gam.setup <- gam(model, data = full.df[full.obs.rows, ], fit = FALSE)
  ## Number of smooth terms in the formula.
  n.smooth <- length(gam.setup$smooth)
  ## Setting up list for column indicators.
  r.cols <- vector(mode = "list", length = n.smooth)
  ## Building design matrices from data.
  if (build.sm){
    ## Setting up sm and sm2ran objects. One component for each smooth
    ## term.
    sm <- sm2ran <- vector(mode = "list", length = n.smooth)
    for (i in 1:n.smooth){
      ## Smooth object.
      sm[[i]] <- gam.setup$smooth[[i]]
      ## The label for this smooth object.
      l <- sm[[i]]$label
      ## Finding which columns in the design matrix correspond to
      ## this smooth.
      r.cols[[i]] <- which(l == substr(gam.setup$term.names, 1, nchar(l)))
      ## Adding the design matrix for this smooth term to its smooth
      ## object. For some reason it isn't bundled in there already.
      sm[[i]]$X <- gam.setup$X[, r.cols[[i]]]
      ## Doing the smooth2random() magic.
      sm2ran[[i]] <- smooth2random(sm[[i]], "", type = 2)
      colnames(sm2ran[[i]]$Xf) <- paste0(l, ".", 1:ncol(sm2ran[[i]]$Xf))
    }
  } else {
    Z.sm <- X.sm <- vector(mode = "list", length = n.smooth)
    ## Building design matrices for new data based on an
    ## already fitted model.
    for (i in 1:n.smooth){
      ## The label for this smooth object.
      l <- sm[[i]]$label
      ## Finding which columns in the design matrix correspond to
      ## this smooth.
      r.cols[[i]] <- which(l == substr(gam.setup$term.names, 1, nchar(l)))
      ## If sm and sm2r objects provided, then we can generate
      ## design matrices like this.
      r <- s2rPred(sm[[i]], sm2ran[[i]], data = df)
      Z.sm[[i]] <- r$rand$Xr
      X.sm[[i]] <- r$Xf
      colnames(X.sm[[i]]) <- paste0(l, ".", 1:ncol(X.sm[[i]]))
    }
  }
  ## Rows in df that have fully observed covariates.
  obs.rows <- apply(df, 1, function(x) !any(is.na(x)))
  if (build.sm){
    X.lbm.fixed <- gam.setup$X[, -unlist(r.cols), drop = FALSE]
    ## Getting the columns of the fixed-effects design matrix that
    ## correspond to unpenalised basis functions, which has something
    ## to do with null spaces.
    X.lbm.rand <- do.call(cbind, lapply(sm2ran, function(x) x$Xf))
    ## Creating a design matrix for penalised basis functions.
    Z.lbm.obs <- do.call(cbind, lapply(sm2ran, function(x) x$rand$Xr))
  } else {
    ## Rows in orig.df that have fully observed covariates.
    orig.obs.rows <- apply(orig.df, 1, function(x) !any(is.na(x)))
    ## Rows of the full data frame that correspond to the data we are
    ## creating the matrices for.
    df.rows <- (sum(orig.obs.rows) + 1):(sum(orig.obs.rows) + sum(obs.rows))
    ## Getting the columns of the design matrix that correspond to
    ## fixed effects.
    X.lbm.rand <- do.call(cbind, X.sm)[obs.rows, ]
    Z.lbm.obs <- do.call(cbind, Z.sm)[obs.rows, ]
    X.lbm.fixed <- gam.setup$X[df.rows, -unlist(r.cols)]
  }
  ## Combining the fixed-effects columns of the design matrix with
  ## the unpenalised basis functions.
  X.lbm.obs <- cbind(X.lbm.fixed, X.lbm.rand)
  ## Number of basis functions.
  n.u <- sapply(sm2ran, function(x) ncol(x$rand$Xr))
  ## An indicator for which penalised basis functions correspond to
  ## which smooth term in the model formula.
  u.fac <- rep(1:n.smooth, times = n.u)
  ## If we have NAs then gam() removes them, so we need to put those
  ## rows back in.
  X.lbm <- matrix(NA, nrow = nrow(df), ncol = ncol(X.lbm.obs))
  X.lbm[obs.rows, ] <- X.lbm.obs
  colnames(X.lbm) <- colnames(X.lbm.obs)
  Z.lbm <- matrix(NA, nrow = nrow(df), ncol = ncol(Z.lbm.obs))
  Z.lbm[obs.rows, ] <- Z.lbm.obs
  ## Combining everything we need in a list.
  list(X.lbm = X.lbm, Z.lbm = Z.lbm, sm = sm, sm2ran = sm2ran, u.fac = u.fac)
}


## Fit a model with smooth effects.

## capt:       A list, where each component is the capture histories for a
##             session.
## traps:      A list, where each component is the trap locations for a
##             session.
## mask:       A list, where each component is the mask object for a session
## fixed:      A formula object, specifying the fixed effects, for which
##             smoothing will not apply.
## smooth:     A list, where each component provides a smooth
##             specification object.
## mask.df:    A data frame with spatial variables, in the same format as
##             the mask object.
## pred.df:    A data frame with spatial variables to create predicted
##             density estimates for.
## unsuitable: A list specifying unsuitable habitat. Each component
##             name should be a categorical variable name, and the
##             component itself should be a vector specifying the
##             levels of the variable that correspond to unsuitable
##             habitat.
## detfn:      String indicating detection function type. "HN" or "HHN"
## tmb.dir:    File path to the tmb directory.
fit.scr.smooth <- function(capt, 
                           traps, 
                           mask.full, 
                           model, 
                           mask.df, 
                           pred.df, 
                           unsuitable = NULL, 
                           n.occasions = 1, 
                           start.par,
                           detfn = "HN",
                           tmb.dir = "tmb"){
  ## Compiling TMB code.
  if (detfn == "HN"){
    compile(paste(tmb.dir, "gpscr_hn.cpp", sep = "/"))
    dyn.load(dynlib(paste(tmb.dir, "gpscr_hn", sep = "/")))
    DLL <- "gpscr_hn"
  }
  if (detfn == "HHN"){
    compile(paste(tmb.dir, "gpscr_hazard.cpp", sep = "/"))
    dyn.load(dynlib(paste(tmb.dir, "gpscr_hazard", sep = "/")))
    DLL <- "gpscr_hazard"
  }
  
  ## A list full of arguments.
  args <- as.list(environment())
  ## Some data wrangling.
  n.sessions <- length(capt)
  mask.areas <- numeric(n.sessions)

  ## Bearings?
  if ("bearing" %in% names(capt[[1]])){
    do_bearing = 1
    tmb_map = NULL
  } else{
    do_bearing = 0
    tmb_map = list(link_kappa = as.factor(NA))
  }

  bin.capt <- bearing.capt <- mask.dists <- mask.bearings <-
    vector("list", length = n.sessions)
  ## Number of variables in unsuitable list.
  n.unsuitable <- length(unsuitable)
  for (i in 1:n.sessions){
    mask.full[[i]] <- as.data.frame(mask.full[[i]])
    ## Calculating the area of a mask cell.
    mask.areas[i] <- min(calc.dists(mask.full[[i]][1, , drop = FALSE],
                                    mask.full[[i]][-1, ]))^2/10000
    ## Removing unsuitable habitat from mask and data frame.
    if (!is.null(unsuitable)){
      for (j in 1:n.unsuitable){
        remove.cells <- which(mask.df[[i]][[names(unsuitable)[j]]] %in%
                              unsuitable[[j]])
        mask.full[[i]] <- mask.full[[i]][-remove.cells, ]
        mask.df[[i]] <- mask.df[[i]][-remove.cells, ]
      }
    }
  }
  for (i in 1:n.sessions){
    bin.capt[[i]] <- capt[[i]]$bincapt
    mask.dists[[i]] <- calc.dists(traps[[i]], mask.full[[i]])
    if (do_bearing == 1){
      bearing.capt[[i]] <- capt[[i]]$bearing
      mask.bearings[[i]] <- t(calc.bearings(traps[[i]], mask.full[[i]]))
    } else{
      bearing.capt = list(matrix(1))
      mask.bearings = list(matrix(1))
    }
  }
  n.sessions <- length(capt)
  all.mask <- do.call(rbind, mask.full)
  all.mask.df <- do.call(rbind, mask.df)
  all.mask.df <- cbind(all.mask, all.mask.df)
  df <- rbind(all.mask.df, pred.df)
  n.pred <- nrow(pred.df)
  ## Setting up design matrices for fixed and random effects.
  design.mats <- construct.design(model = model, df = df)
  X.lbm <- design.mats$X.lbm
  Z.lbm <- design.mats$Z.lbm
  u.fac <- design.mats$u.fac
  sm <- design.mats$sm
  sm2ran <- design.mats$sm2ran
  ## Sorting out objects for TMB.
  n.mask <- sapply(mask.full, nrow)
  n.mask.pred <- c(n.mask, n.pred)
  X <- split(as.data.frame(X.lbm), rep(1:(n.sessions + 1), times = n.mask.pred))
  X <- lapply(X, as.matrix)
  Z <- split(as.data.frame(Z.lbm), rep(1:(n.sessions + 1), times = n.mask.pred))
  Z <- lapply(Z, as.matrix)
  mask.X <- X[1:n.sessions]
  mask.Z <- Z[1:n.sessions]
  pred.X <- as.matrix(X[[n.sessions + 1]])
  pred.Z <- as.matrix(Z[[n.sessions + 1]])
  ## Add starting values for random effects if not there. Assume
  ## start.par$D_betas is length 1, intercept.
  if (length(start.par$D_betas) == 1){
    start.par$D_betas = c(start.par$D_betas, rep(0, ncol(pred.X) - 1))
  }
  if (!("link_kappa" %in% names(start.par))){
    start.par$link_kappa = log(20)
  }
  if (!("link_sigma_u" %in% names(start.par))){
    start.par$link_sigma_u = rep(log(1), max(u.fac))
  }
  if (!("u" %in% names(start.par))){
    start.par$u = rep(0, ncol(pred.Z))
  }
  ## Making TMB object.
  gpscr <- MakeADFun(data = list(n_sessions = n.sessions,
                                 all_n_dets = sapply(bin.capt, nrow),
                                 all_capt = bin.capt,
                                 all_bearing = bearing.capt,
                                 all_traps = traps,
                                 all_n_traps = sapply(traps, nrow),
                                 all_mask_dists = mask.dists,
                                 all_mask_bearings = mask.bearings,
                                 all_n_mask = sapply(mask.full, nrow),
                                 all_mask_areas = mask.areas,
                                 all_X_mm = mask.X,
                                 all_Z_mm = mask.Z,
                                 pred_X = pred.X,
                                 pred_Z = pred.Z,
                                 u_fac= u.fac,
                                 do_smooth = 1,
                                 do_bearing = do_bearing,
                                 n_occs = n.occasions
                                 ),
                     parameters = start.par,
                     random = "u",
                     map = tmb_map,
                     DLL = DLL)    ## Fitting model and getting standard errors.
  cat("\n AD Fun complete.  Starting fit. \n")
  fit <- nlminb(gpscr$par, gpscr$fn, gpscr$gr,
                control = list(eval.max = 500,
                               iter.max = 500))
  cat("\n Fit complete. Starting sdreport() \n")
  sdrep <- sdreport(gpscr, getJointPrecision = TRUE)
  list(fit = fit, sdrep = sdrep, sm = sm, sm2ran = sm2ran, u.fac = u.fac, df = df, args = args)
}

## Simulate capture histories and bearings, with animal density
## varying according to a covariate.
##
## traps:  A list of matrices, each specifying trap locations for a
##         session.
## mask:   A mask object specifying cell locations for the whole region.
## df:     A data frame of covariates, with a row for each mask cell.
## D.calc: A function that takes df as an argument, and returns animal
##         density for each row.
## pars:   A vector of parameter values for lambda0/g0, sigma, and kappa,
##         in that order.
## detfn:  String indicating detection function. "HN" or "HHN"
sim.capt.cov <- function(traps, 
                         mask, 
                         df, 
                         D.calc, 
                         pars,
                         detfn){
  ## Extracting parameters.
  if (detfn == "HN"){
    g0 <- pars[1]
  }
  if (detfn == "HHN"){
    lambda0 <- pars[1]
  }
  sigma <- pars[2]
  kappa <- pars[3]
  
  ## Number of sessions.
  n.sessions <- length(traps)
  ## Number of traps in each session.
  n.traps <- sapply(traps, nrow)
  ## Calculating density for every mask cell.
  D <- D.calc(df)
  ## Number of cells in the mask.
  n.cells <- nrow(mask)
  ## Width of each cell, assuming they're in a grid and the first
  ## two are adjacent.
  cell.width <- max(abs(c(mask[1, 1] - mask[2, 1], mask[1, 2] - mask[2, 2])))
  ## Cell area in square metres.
  cell.area <- cell.width^2
  ## Cell area in hectares.
  cell.area.ha <- cell.area/10000
  ## Expected number of animals in each cell.
  exp.n.per.cell <- D*cell.area.ha
  ## Simulating number of animals in each cell.
  n.per.cell <- rpois(n.cells, exp.n.per.cell)
  ## Animal locations, at first in the centre of each cell.
  s <- cbind(rep(mask[, 1], n.per.cell), rep(mask[, 2], n.per.cell))
  ## Total number of animals.
  n.animals <- nrow(s)
  ## Jittering so that the locations are uniform within cells.
  s[, 1] <- s[, 1] + runif(n.animals, -cell.width/2, cell.width/2)
  s[, 2] <- s[, 2] + runif(n.animals, -cell.width/2, cell.width/2)
  ## Distances between animals and traps.
  dists <- lapply(traps, function(x) calc.dists(s, x))
  ## Bearings between animals and traps.
  bearings <- lapply(traps, function(x) t(calc.bearings(x, s)))
  ## Creating capture histories.
  capt <- vector(mode = "list", length = n.sessions)
  for (i in 1:n.sessions){
    if (detfn == "HN"){
      det.probs <- g0*exp(-dists[[i]]^2/(2*sigma^2))
    }
    if (detfn == "HHN"){
      det.probs <- 1 - exp(-lambda0*exp(-dists[[i]]^2/(2*sigma^2)))
    }
    capt.sess <- matrix(rbinom(n.animals*n.traps[i], 1, det.probs),
                        nrow = n.animals, byrow = TRUE)
    bearing.sess <- matrix(0, nrow = n.animals, ncol = n.traps[i])
    dets <- which(capt.sess == 1, arr.ind = TRUE)
    for (j in seq_len(nrow(dets))){
      bearing.sess[dets[j, 1], dets[j, 2]] <-
        rvm(1, bearings[[i]][dets[j, 1], dets[j, 2]], kappa)
    }
    detected <- apply(capt.sess, 1, function(x) any(x == 1))
    capt[[i]] <- list(bincapt = capt.sess[detected, , drop =  FALSE],
                      bearing = bearing.sess[detected, , drop = FALSE])
  }
  list(capt = capt, s = s)
}

## A function for plotting covariates and estimated surfaces.
plot.surf <- function(x, y, z, traps = NULL, trap.dets = NULL, scale = NA, scale.loc = "topleft", cols = NULL, zlim = NULL, sub = NULL, cex.mult = 1, cex.main.mult = 1, legend = TRUE, title = "Density", ...){
  xlim <- range(x)
  ylim <- range(y)
  xs <- sort(unique(x))
  ys <- sort(unique(y))
  nx <- length(xs)
  ny <- length(ys)
  ix <- match(x, xs)
  iy <- match(y, ys)
  z.mat <- matrix(NA, nrow = nx, ncol = ny)
  z.mat[cbind(ix, iy)] <- z
  plot.new()
  plot.window(xlim = xlim, ylim = ylim, asp = 1)
  if (legend){
    image.fun <- image.plot
  } else {
    image.fun <- image
  }
  if (!is.null(trap.dets)){
    min.cex <- 1
    max.cex <- 2
    trap.dets.zt <- trap.dets[trap.dets > 0]
    trap.cex <- rep(1, length(trap.dets))
    trap.cex[trap.dets > 0] <- cex.calc(trap.dets.zt, min(trap.dets.zt), max(trap.dets.zt),
                                        min.cex, max.cex)
  }
  if (is.numeric(z)){
    if (is.null(cols)){
      cols <- viridis(100)
    }
    image.fun(xs, ys, z.mat, col = cols, add = TRUE, legend.shrink = 0.5, zlim = zlim)
  } else {
    if (is.null(cols)){
      cols <- brewer.pal(length(unique(z)), "Greens")
    }
    image(xs, ys, z.mat, col = cols, add = TRUE)
    legend.arg <- levels(z)
    fill.arg <- cols
    pch.arg <- rep(NA, length(cols))
    cex.arg <- rep(1, length(cols))
    if (!is.null(trap.dets)){
      gpd.ex <- c(1, 3)
      legend.arg <- c(legend.arg,
                      paste(gpd.ex[1], "detection per day"),
                      paste(gpd.ex[2], "detections per day"),
                      "No detections")
      fill.arg <- c(fill.arg, NA, NA, NA)
      pch.arg <- c(pch.arg, 16, 16, 1)
      cex.arg <- c(cex.arg,
                   ## Multiplying number of groups detected per
                   ## day by 3 because there are three days.
                   cex.calc(3*gpd.ex[1], min(trap.dets.zt), max(trap.dets.zt), min.cex, max.cex),
                   cex.calc(3*gpd.ex[2], min(trap.dets.zt), max(trap.dets.zt), min.cex, max.cex),
                   1)
    }
    legend(xlim[2] - 0.1*(xlim[2] - xlim[1]), ylim[2], legend = legend.arg, fill = fill.arg, pch = pch.arg,
           pt.cex = cex.arg*cex.mult, border = NA, bty = "n", xpd = NA)
  }
  title(title, ..., cex.main = cex.main.mult*1, adj = 0)
  mtext(sub, side = 1, line = -0.5, cex = 0.6, adj = 0)
  if (!is.null(traps)){
    if (is.null(trap.dets)){
      trap.pch <- 16
      trap.cex <- 1
    } else {
      trap.pch <- ifelse(trap.dets > 0, 16, 1)
    }
    points(traps, pch = trap.pch, cex = trap.cex*cex.mult)
  }
  if (!is.na(scale)){
    if (scale.loc == "topleft"){
      lines(c(xlim[1], xlim[1], xlim[1] + scale, xlim[1] + scale),
            c(ylim[2] - scale/10, ylim[2], ylim[2], ylim[2] - scale/10))
      text(xlim[1] + scale/2, ylim[2], paste(scale/1000, "km"), adj = c(0.5, 1.25))
    } else if (scale.loc == "topright"){
      lines(c(xlim[2], xlim[2], xlim[2] - scale, xlim[2] - scale),
            c(ylim[2] - scale/10, ylim[2], ylim[2], ylim[2] - scale/10))
      text(xlim[2] - scale/2, ylim[2], paste(scale/1000, "km"), adj = c(0.5, 1.25))
    }
  }
}

cex.calc <- function(n.dets, min.dets, max.dets, min.cex, max.cex){
  sqrt.n <- sqrt(n.dets)
  sqrt.min <- sqrt(min.dets)
  sqrt.max <- sqrt(max.dets)
  s.tilde <- (sqrt.n - sqrt.min)/(sqrt.max - sqrt.min)
  min.cex + s.tilde*(max.cex - min.cex)
}
