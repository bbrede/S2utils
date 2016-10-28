#' Simulate sensor
#' 
#' Spectral sensor simulation
#' 
#' @description Assume the simulating sensor has more bands than target sensor. 
#' 
#' @param simulator Simulator/emulator
#' @param target Simulated sensor
#' @param rho_eps The simulator and the target need to share at least one wavelength for which both their responses are higher than rho_eps
#' 
#' @return weights to apply to the bands of src to simulate target; as matrix
#' 
#' @author Benjamin Brede
#' 
#' @export

Sensor_sim <- function(simulator, target, rho_eps) {
  
  # check if sensor specs are same
  if (
    attr(simulator, 'wlunit') != attr(target, 'wlunit') |
      attr(simulator, 'minwl') != attr(target, 'minwl') |
      attr(simulator, 'maxwl') != attr(target, 'maxwl') |
      attr(simulator, 'stepsize') != attr(target, 'stepsize'))
    stop('Sensor specs not the same!')
  
  # lambda <- seq(attr(simulator, 'minwl'), attr(simulator, 'maxwl'), attr(simulator, 'stepsize'))
  
  # simulate bands (as matrix) with given weights (vector) and compare with trg (vector)
  band_error <- function(weights, simu, trg) {
    sim <- colSums(t(simu) * weights, na.rm = TRUE) / sum(weights)
    # normalize in respect to maximum value
    sim_norm <- sim / max(sim, na.rm = TRUE)
    
    # weight error with response of target => high errors at high response become more important
    sum((sim_norm - trg) ^ 2 * trg, na.rm = TRUE)
    ## sum((sim_norm - trg) ^ 2, na.rm = TRUE)
  }  
  
  # treating simulator as matrix is more convenient
  simu <- as.matrix(simulator)
  
  # apply per target band
  sapply(names(target), function(trg_band) {    
        
    trg <- target[,trg_band]
    
    # bands that are suitable to simulate trg_band (set all non-releveant bands to NA; non-relevant = bands that don't overlap anywhere in the spectrum)
    is_suitable <- apply(simu, MARGIN = 2, function(col) {
      # test if any responses overlap between col of simulator and trg_band
      any(col > rho_eps & trg > rho_eps)
    })
    suitable_num <- sum(is_suitable)
    
    res <- rep(NA, ncol(simulator))
    names(res) <- names(simulator)
    
    # cases that have x suitable bands for simulation 
    # ... none
    if (suitable_num == 0) {
      res
    }
    
    # ... 1
    else if (suitable_num == 1) {
      res[is_suitable] <- 1
      res
    }
    
    # ... multiple bands
    else {
      
      simu_select <- simulator[,is_suitable]
      
      # define error function for this target band
      error <- function(weights) band_error(weights, simu_select, trg)
      
      # par: start weight for each band = 1
      # lower: do not allow weigths to be negative
      opt <- optim(par = rep(1, ncol(simu_select)), 
                   fn = error,
                   lower = rep(0, ncol(simu_select)), 
                   method = 'L-BFGS-B')
      
      # set weights in the results vector
      res[is_suitable] <- opt$par
      
      # it's possible that simulator bands got non-significant weights (equal 0) during optimization
      res[res == 0] <- NA
      
      # in case only one band is significant, set this to 1            
      if (sum(res != 0, na.rm = TRUE) == 1)
        res / res
      else
        res
    }
  })
}