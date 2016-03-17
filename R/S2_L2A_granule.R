#' Translate S2 L2A products
#' 
#' Translate a single S2 L2A granule.
#' 
#' @param granule_folder Folder that contains the S2 L2A granule
#' @param band Band to extract, can be spectral (B02 to B12 plus B8A) or thematic (SCL or CLD)
#' @param resolution Band resolution in m
#' @param ... Additional arguments as for \code{\link{writeRaster}}
#' 
#' @return RasterLayer
#' 
#' @details rasterOptions apply for writing the return raster
#' 
#' @export
#' 
#' @import raster
#' @import gdalUtils
#' @import rgdal


S2_L2A_granule <- function(granule_folder, band, resolution=c(10, 20, 60)) {
  
  library(raster)
  library(gdalUtils)
  library(rgdal)
  
  # extract corner coordinates, cell size, CRS from xml
  xml <- list.files(granule_folder, 'S2A.+xml$', full.names = TRUE, recursive = FALSE)
  geo_info <- S2_extract_geoinfo(xml, resolution)
  
  jp2 <- list.files(path = granule_folder,
                    pattern = paste0(band, '_.*', resolution, 'm.jp2'),
                    full.names = TRUE, recursive = TRUE)
  
  r <- gdal_translate(src_dataset = jp2, dst_dataset = rasterTmpFile(), output_Raster = TRUE)
  
  # assign CRS and extent
  crs(r) <- CRS(paste0('+init=', tolower(geo_info$EPSG)))
  extent(r) <- extent(c(geo_info$UL_corner$ULX,
                        geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
                        geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
                        geo_info$UL_corner$ULY))      
  
  # write raster, so the crs and extent are written to file
  writeRaster(r, ...)

  
  ### GDALWARP - did not work
  # target extent in UTM
  #te <- c(geo_info$UL_corner$ULX, 
  #        geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
  #        geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
  #        geo_info$UL_corner$ULY)
  #gdalwarp(jp2, dst, t_srs = geo_info$EPSG, te = te) 
}