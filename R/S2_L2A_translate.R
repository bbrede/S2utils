#' Translate S2 L2A products
#' 
#' Translate a S2 L2A scene into other raster format with CRS.
#' 
#' @description Single granules are mosaiced to form one output raster.
#' 
#' @param S2_folder Folder that contains the S2 L2A product (typically suffixed with .SAFE)
#' @param band Band to extract, can be spectral (B01 to B12 plus B8A) or thematic (SCL or CLD)
#' @param resolution Band resolution in m
#' @param ... Additional arguments as for \code{\link{writeRaster}}
#' 
#' @return RasterLayer corresponding to the mosaiced raster of the specified band and resolution
#' 
#' @details rasterOptions apply for writing the return raster
#'
#' @export
#' 
#' @import raster
#' @import gdalUtils
#' @import rgdal


# author: Benjamin Brede
# date: 2016-01-16


S2_L2A_translate <- function(S2_folder, band, resolution=c(10, 20, 60), ...) {
  
  library(raster)
  library(gdalUtils)
  library(rgdal)
    
  # TODO: require gdal version >= 2.1 -> utils::compareVersion 
  
  # list all granule folders
  all_granules <- list.dirs(file.path(S2_folder, 'GRANULE'), full.names = FALSE, recursive = FALSE) 
  
  # extract the single tiles
  rsts <- lapply(all_granules, S2_L2A_granule, band = band, resolution = resolution)
  rst_names <- sapply(rsts, filename)
  
  r <- mosaic_rasters(rst_names, dst_dataset = rasterTmpFile(), output_Raster = TRUE)
  
  writeRaster(r, ...)
}