#' Translate S2 L2A products
#' 
#' Translate a single S2 L2A granule.
#' 
#' @param granule_path Folder that contains the S2 L2A granule (typically within GRANULE folder and of form S2x_USER_MSI_L2A_..._Nxx.xx)
#' @param band Band to extract, can be spectral (B02 to B12 plus B8A) or thematic (SCL or CLD)
#' @param resolution Band resolution in m
#' @param filename Output filename; will be automatically suffixed with tileID, band name and resolution
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
#' @import tools
#' @import raster
#' @import gdalUtils
#' @import rgdal
#' @import utils


S2_L2A_granule <- function(granule_path, band, resolution=c(10, 20, 60), filename, ...) {
  
  library(tools)
  library(raster)
  library(gdalUtils)
  library(rgdal)
  library(utils)
  
  # extract corner coordinates, cell size, CRS from xml
  xml_file <- list.files(granule_path, 'S2A.+xml$', full.names = TRUE, recursive = FALSE)
  geo_info <- S2_extract_geoinfo(xml_file, resolution)
  
  jp2 <- list.files(path = granule_path,
                    pattern = paste0(band, '_.*', resolution, 'm.jp2'),
                    full.names = TRUE, recursive = TRUE)
  
  # extract date and time from filename
  datetime <- as.POSIXct(strptime(sub('.*S2.*_V([0-9]{8}T[0-9]{6})_.*jp2', '\\1', jp2), '%Y%m%dT%H%M%S', 'UTC'))
  # extract tile ID from filename
  tileID <- sub('.*_(T[0-9]{2}[A-Z]{3})_.*jp2', '\\1', jp2)
  
  # insert tileID in filename
  mod_filename <-  paste0(file_path_sans_ext(filename), '_', tileID, '_', band, '_', resolution, 'm.', file_ext(filename))
  
  # pre-check overwriting options to prevent unnecessary operations
  optsOverwrite <- as.logical(sub('.*(TRUE|FALSE).*', '\\1', grep('overwrite', capture.output(rasterOptions()), value = TRUE)))
  args <- list(...)
  
  if ('overwrite' %in% names(args))
    overwrite <- args$overwrite
  else
    overwrite <- optsOverwrite
  
  if (file.exists(mod_filename) & overwrite | !file.exists(mod_filename)) {
    
    r <- gdal_translate(src_dataset = jp2, dst_dataset = rasterTmpFile(), output_Raster = TRUE)
    
    # assign CRS and extent
    crs(r) <- CRS(paste0('+init=', tolower(geo_info$EPSG)))
    extent(r) <- extent(c(geo_info$UL_corner$ULX,
                          geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
                          geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
                          geo_info$UL_corner$ULY))
    
    # write raster, so the crs and extent are written to file
    out <- writeRaster(r, filename = mod_filename, ...)
    
  } else {
    out <- raster(mod_filename)
  }
  
  # add Date
  out <- setZ(out, datetime, 'DateTime')
  
  out
  
  ### GDALWARP - did not work
  # target extent in UTM
  #te <- c(geo_info$UL_corner$ULX, 
  #        geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
  #        geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
  #        geo_info$UL_corner$ULY)
  #gdalwarp(jp2, dst, t_srs = geo_info$EPSG, te = te) 
}