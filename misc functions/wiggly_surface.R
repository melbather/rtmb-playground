# function to simulate wiggly surface
wiggly_surface <- function(mask, cov.pars, beta0, seed) {
  set.seed(seed)
  exp(
    geoR::grf(
      nrow(mask),
      grid = mask, 
      xlims = range(mask$x),
      ylims = range(mask$y), 
      cov.pars = cov.pars
    )$data + beta0
  )
}

# modify the function above to simulate wiggly surfaces for two species
wiggly_surface_multi <- function(
  mask, 
  cov.pars, 
  beta0, 
  beta1, 
  alpha0, 
  alpha1, 
  alpha2,
  seed1,
  seed2
) {
  set.seed(seed1)

  field1 <- geoR::grf(
      nrow(mask),
      grid = mask, 
      xlims = range(mask$x),
      ylims = range(mask$y), 
      cov.pars = cov.pars
    )$data
  
  set.seed(seed2)
  
  field2 <- geoR::grf(
      nrow(mask),
      grid = mask, 
      xlims = range(mask$x),
      ylims = range(mask$y), 
      cov.pars = cov.pars
    )$data

  D1 <- exp(beta1 * field1 + beta0)
  D2 <- exp(alpha1 * field1 + alpha2 * field2 + alpha0)

  list(
    D1 = D1,
    D2 = D2
  )
}