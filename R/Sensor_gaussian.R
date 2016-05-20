#' Define sensor with gaussian response function
#' 
#' Define sensor with gaussian response function
#' 
#' @param center Numeric vector with center wavelengths
#' @param fwhm Numeric vector with Full Width Half Maximum values
#' @param []
#' 
#' @return [weights to apply to the bands of src to simulate template; as data.frame]
#' 
#' @export
#' 
#' @import []

Sensor_gaussian <- function(center, fwhm, band_name, wlunit='nm', minwl=400, maxwl=2500, stepsize=1) {
  
  if (length(unique(sapply(list(center, fwhm, band_name), length))) != 1)
    stop('Not all band specs have same lengths!')
  
  # all possible wavelengths
  lambda <- seq(minwl, maxwl, stepsize)
  
  gaussian_response <- function(cen, f) exp(-4 * log(2) * (lambda - cen) ^ 2 / f ^ 2)
  
  # create data.frame band by band
  sensor <- do.call(cbind, lapply(seq_along(band_name), function(i) {
    
    response <- gaussian_response(center[i], fwhm[i])
    # set minor responses 0
    response[response < 0.001] <- 0
    
    df <- data.frame(response)
    names(df) <- band_name[i]
    
    df
  }))
  
  # set attributes
  attr(sensor, 'wlunit') <- wlunit
  attr(sensor, 'minwl') <- minwl
  attr(sensor, 'maxwl') <- maxwl
  attr(sensor, 'stepsize') <- stepsize
  
  sensor
}