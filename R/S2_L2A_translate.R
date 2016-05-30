#' Translate S2 L2A products
#' 
#' Translate an S2 L2A scene into other raster format.
#' 
#' @param S2_folder Folder that contains the S2 L2A product (typically suffixed with .SAFE)
#' @param band Band to extract, can be spectral (B01 to B12 plus B8A) or thematic (SCL or CLD)
#' @param resolution Band resolution in m
#' @param filename Output filename, single tiles will be suffixed with tileID
#' @param ... Additional arguments as for \code{\link{writeRaster}}
#' 
#' @return List of RasterLayers corresponding to the raster of the specified band and resolution
#' 
#' @details rasterOptions apply for writing the return rasters
#'
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import raster
#' @import gdalUtils
#' @import rgdal


# author: Benjamin Brede
# date: 2016-01-16


S2_L2A_translate <- function(S2_folder, band, resolution=c(10, 20, 60), filename, ...) {
      
  # TODO: require gdal version >= 2.1 -> utils::compareVersion 
  
  # list all granule folders
  all_granules <- list.dirs(file.path(S2_folder, 'GRANULE'), full.names = TRUE, recursive = FALSE) 
  
  # extract the single tiles
  lapply(all_granules, S2_L2A_granule, band = band, resolution = resolution, filename = filename, ...)
}