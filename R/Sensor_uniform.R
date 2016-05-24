#' Define sensor with uniform response function
#' 
#' Define sensor with uniform response function
#' 
#' @param min Numeric vector with minimum (lower) wavelengths
#' @param max Numeric vector with maximum (upper) wavelengths
#' @param band_name Vector with band names (will be coerced to character)
#' @param wlunit Label of wavelength unit (character)
#' @param minwl Minimum wavelength (numeric)
#' @param maxwl Maximum wavelength (numeric)
#' @param stepsize Wavelength increment (numeric)
#' 
#' @return Data.frame with bands in columns, meta data as attributes
#' 
#' @export

Sensor_uniform <- function(min, max, band_name, wlunit='nm', minwl=400, maxwl=2500, stepsize=1) {
  
  # all possible wavelengths
  lambda <- seq(minwl, maxwl, stepsize)
  
  # create data.frame band by band
  sensor <- do.call(cbind, lapply(seq_along(band_name), function(i) {
    
    response <- numeric(length = length(lambda))
    # set response to 1
    response[lambda >= min[i] & lambda <= max[i]] <- 1
    
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