#' Translate S2 L2A products
#' 
#' Translate an S2 L2A scene into other raster format.
#' 
#' @param S2_folder Folder that contains the S2 L2A product (typically suffixed with .SAFE)
#' @param band Band to extract, can be spectral (B02 to B12 plus B8A) or thematic (SCL or CLD)
#' @param resolution Band resolution in m, allowed: 10, 20, 60
#' @param filename Output filename; will be automatically suffixed with tileID, band name and resolution
#' @param overwrite Overwrite existing files?
#' @param ... Additional arguments as for \code{\link{gdal_translate}}, not allowed: a_srs, a_ullr.
#' 
#' @return List of RasterLayers corresponding to the raster of the specified band and resolution
#'
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import raster
#' @import gdalUtils
#' 
#' @examples 
#' # select appropriate GDAL installtion that can read JPG2000
#' gdal_setInstallation('/usr/bin/', rescan = TRUE)
#' 
#' S2 <- 'S2A_USER_PRD_MSIL2A_PDMC_20151001T124224_R008_V20151001T104705_20151001T104705.SAFE'
#' # use co to access creation options for GTiff files as in gdal_translate
#' S2_L2A_translate(S2, 'B8A', 20, 'test.tif', co = c('COMPRESS=LZW'))  

S2_L2A_translate <- function(S2_folder, band, resolution = c(10, 20, 60), filename, overwrite = FALSE, ...) {
      
  # list all granule folders
  all_granules <- list.dirs(file.path(S2_folder, 'GRANULE'), full.names = TRUE, recursive = FALSE) 
  
  # extract the single tiles
  lapply(all_granules, S2_L2A_granule, band = band, resolution = resolution, filename = filename, ...)
}