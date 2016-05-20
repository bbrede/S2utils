#' Simulate sensor
#' 
#' Spectral sensor simulation
#' 
#' @description Assume the simulating sensors has more bands than target sensor.
#' 
#' @param simulator Simulator
#' @param target Simulated sensor
#' 
#' @return [weights to apply to the bands of src to simulate template; as data.frame]
#' 
#' @export
#' 
#' @import []

Sensor_sim <- function(simulator, target, sig_response) {
  
  
  # TO DO: check sensor specs are same
  
  
  lambda <- seq(attr(simulator, 'minwl'), attr(simulator, 'maxwl'), attr(simulator, 'stepsize'))
  
  # simulate bands (as matrix) with given weights (vector) and compare with trg (vector)
  # output = vector
  band_error <- function(weights, simu, trg) {
    sim <- colSums(t(simu) * weights, na.rm = TRUE) / sum(weights)
    # normalize in respect to maximum value
    sim_norm <- sim / max(sim, na.rm = TRUE)
    
    # weight error with response of target => high errors at high response become more important
    sum((sim_norm - trg) ^ 2, na.rm = TRUE)
  }  
  
  sapply(names(target), function(trg_band) {    
    
    simu <- as.matrix(simulator)
    trg <- target[,trg_band]
    
    # preselect bands (set all non-releveant bands to NA; non-relevant = bands that don't overlap anywhere in the spectrum)
    simu_select <- apply(simu, MARGIN = 2, function(col) {
      # test if any responses overlap
      if (any(col > sig_response & trg > sig_response))
        col
      # otherwise return NAs
      else
        rep(NA, length(col))
    })
    
    # bands that are suitable
    suitable <- !is.na(simu_select[1,])
    suitable_no <- sum(suitable)
    
    # cases that have x suitable bands for simulation 
    # ... none
    if (suitable_no == 0) {
      x <- rep(NA, ncol(simulator)) #matrix(NA, nrow = ncol(simulator), ncol = ncol(target))
      names(x) <- names(simulator)
      x
    }
    
    # ... 1
    else if (suitable_no == 1) {
      as.numeric(suitable)
    }
    
    # ... multiple bands
    else {
      # define error function for this target band
      error <- function(weights) band_error(weights, simu_select, trg)
      
      # par: start weight for each band = 1
      # lower: do not allow weigths to be negative
      opt <- optim(par = rep(1, ncol(simulator)), 
                   fn = error,
                   lower = rep(0, ncol(simulator)), 
                   method = 'L-BFGS-B')
      w <- opt$par
      # (rare) case that bands that overlap with target band have non-significant weights
      if (sum(w != 0) == 1)
        as.numeric(w != 0)
      else
        w
    }
  })
  
  
  
}