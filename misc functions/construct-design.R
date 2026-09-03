library(mgcv)
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

## Example use.
## Some data:
df <- data.frame(x1 = runif(20), x2 = runif(20), x3 = rep(c("A", "B"), 10))
design <- construct.design(~ x1 + s(x2, k = 5) + x3, df = df)
## Here is the fixed-effects design matrix.
X.lbm <- design$X.lbm
## Here is the random-effects design matrix.
Z.lbm <- design$Z.lbm
## So we have four beta coefficients:
## - The intercept.
## - A coefficient for x1.
## - A coefficient for the dummy variable for level B of x3.
## - A coefficient for the first basis function for x2 (I don't really
##   understand why this one has a fixed effect rather than a random
##   effect, but that's how it goes.
X.lbm
## And we have three basis functions with random coefficients.
Z.lbm
## The u.fac indiates which smoother each basis function in Z.lbm
## corresponds to. In this case they're all the same value, because
## all the basis functions are for the smooth effect of x2.

## In the below example, we use smooth effects for x1 and x2, so u.fac
## tells us which columns correspond to which smoother.
design <- construct.design(~ s(x1, k = 7) + s(x2, k = 5) + x3, df = df)
design$X.lbm
design$Z.lbm
## The smoother for x1 has two more basis functions than the smoother
## for x2, because we set k = 7 for the former and k = 5 for the
## latter.
design$u.fac
## When we fit penalised splines using the random effect idea, then
## the random u coefficients for each smoother gets a different
## variance parameter. So here the first five random coefficients
## would have standard deviation sigma_u1, while the next three would
## all have standard deviatoin sigma_u2.
