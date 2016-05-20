#' S2 band names
#' 
#' Possible S2 band names and native resolutions
#' 
#' @description Note that derived bands (all bands not starting with B) are produced at the resolution that is demanded, i.e. don't have a native resolution 
#' 
#' @param with_name Band names that should be returned. Defaults to all.
#' @param with_resolution Resolutions that should be returned. Defaults to all.
#' 
#' @return data.frame with available S2 band names and their native resolution
#' 
#' @export


S2_bands <- function(with_name=NULL, with_resolution=NULL) {
  
  #   high <- c('B02', 'B03', 'B04', 'B08')
  #   medium <- c('B05', 'B06', 'B07', 'B8A')
  #   low <- c('B01', 'B09', 'B10')
  #   derived <- c('AOT', 'VIS', 'WVP', 'SCL', 'CLD')
  #   
  #   S2_bands <- data.frame(Name = c(high, medium, low, derived),
  #                          Resolution = c(rep(10, length(high)), rep(20, length(medium)), rep(60, length(low)), rep(NA, length(derived))))
  
  df <- data.frame(Name = c('B01', 'B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B09', 'B10', 'B11', 'B12', 'AOT', 'VIS', 'WVP', 'SCL', 'CLD'),
                   Resolution = c(60, 10, 10, 10, 20, 20, 20, 10, 20, 60, 60, 20, 20, rep(NA, 5)))  
  if (!is.null(with_name))
    df <- subset(df, Name == with_name)
  if (!is.null(with_resolution))
    df <- subset(df, Resolution == with_resolution)
  
  df
}