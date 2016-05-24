#' Define sensor with gaussian response function
#' 
#' Define sensor with gaussian response function
#' 
#' @param center Vector with center wavelengths (numeric vector)
#' @param fwhm Vector with Full Width Half Maximum values (numeric vector)
#' @param band_name Vector with band names (will be coerced to character)
#' @param wlunit Label of wavelength unit (character)
#' @param minwl Minimum wavelength (numeric)
#' @param maxwl Maximum wavelength (numeric)
#' @param stepsize Wavelength increment (numeric)
#' 
#' @return Data.frame with bands in columns, meta data as attributes
#' 
#' @export

Sensor_gaussian <- function(center, fwhm, band_name, wlunit='nm', minwl=400, maxwl=2500, stepsize=1) {
  
  if (length(unique(sapply(list(center, fwhm, band_name), length))) != 1)
    stop('Not all band specs have same lengths!')
  
  # all possible wavelengths
  lambda <- seq(minwl, maxwl, stepsize)
  
  # response function (based on lambda) defined by center wavelength and fwhm of band
  gaussian_response <- function(cen, f) exp(-4 * log(2) * (lambda - cen) ^ 2 / f ^ 2)
  
  # create data.frame band by band
  sensor <- do.call(cbind, lapply(seq_along(band_name), function(i) {
    
    response <- gaussian_response(center[i], fwhm[i])
    # set minor responses 0
    response[response < 0.001] <- 0
    
    df <- data.frame(response)
    names(df) <- as.character(band_name)[i]
    
    df
  }))
  
  # set attributes
  attr(sensor, 'wlunit') <- wlunit
  attr(sensor, 'minwl') <- minwl
  attr(sensor, 'maxwl') <- maxwl
  attr(sensor, 'stepsize') <- stepsize
  
  sensor
}