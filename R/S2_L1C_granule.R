#' Translate S2 L1C products
#' 
#' Translate a single S2 L1C granule.
#' 
#' @param granule_folder Folder that contains the S2 L2A granule
#' @param band Band to extract, can be spectral (B02 to B12 plus B8A) or thematic (SCL or CLD)
#' @param ... Additional arguments as for \code{\link{writeRaster}}
#' 
#' @return RasterLayer
#' 
#' @details rasterOptions apply for writing the return raster
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import raster
#' @import gdalUtils
#' @import rgdal

S2_L1A_granule <- function(granule_folder, band, lazy=FALSE, ...) {
  
  library(raster)
  library(gdalUtils)
  library(rgdal)
  
  args <- list(...)
  
  if (lazy & 'filename' %in% names(args) & file.exists(filename)) {
    
    raster(filename)
    
  } else {
    
    # extract corner coordinates, cell size, CRS from xml
    geo_info <- S2_extract_geoinfo(list.files(granule_folder, 'S2A.+xml$', full.names = TRUE, recursive = FALSE), resolution)
    
    jp2 <- list.files(path = granule_folder,
                      pattern = paste0(band, '.jp2'),
                      full.names = TRUE, recursive = TRUE)
    
    r <- gdal_translate(src_dataset = jp2, dst_dataset = rasterTmpFile(), output_Raster = TRUE)
    
    # assign CRS and extent
    crs(r) <- CRS(paste0('+init=', geo_info$EPSG))
    extent(r) <- extent(c(geo_info$UL_corner$ULX,
                          geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
                          geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
                          geo_info$UL_corner$ULY))      
    
    
    # write raster if filename is supplied
    if ('filename' %in% names(args))
      writeRaster(r, ...)
    else 
      r
    
    ### GDALWARP - did not work
    # target extent in UTM
    #te <- c(geo_info$UL_corner$ULX, 
    #        geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
    #        geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
    #        geo_info$UL_corner$ULY)
    #gdalwarp(jp2, dst, t_srs = geo_info$EPSG, te = te) 
  }
}