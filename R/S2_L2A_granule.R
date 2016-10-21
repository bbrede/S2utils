#' Translate S2 L2A products
#' 
#' Translate a single S2 L2A granule. 
#' 
#' @param granule_path Folder that contains the S2 L2A granule (typically within GRANULE folder and of form S2x_USER_MSI_L2A_..._Nxx.xx)
#' @param band Band to extract, can be spectral (B02 to B12 plus B8A) or thematic (SCL or CLD)
#' @param resolution Band resolution in m, allowed: 10, 20, 60
#' @param filename Output filename; will be automatically suffixed with granuleID, band name and resolution
#' @param overwrite Overwrite existing files?
#' @param ... Additional arguments as for \code{\link{gdal_translate}}, not allowed: a_srs, a_ullr.
#' 
#' @return RasterLayer
#' 
#' @details rasterOptions apply for writing the return raster. Filename will be automatically suffixed with granule ID and band name.
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import tools
#' @import raster
#' @import gdalUtils

S2_L2A_granule <- function(granule_path, band, resolution = c(10, 20, 60), filename, overwrite = FALSE, ...) {
  
  # extract corner coordinates, cell size, CRS from xml
  xml_file <- list.files(granule_path, 'S2A.+xml$', full.names = TRUE, recursive = FALSE)
  if (length(xml_file) == 0)
    stop(paste('Metadata file (xml) not found for granule:', granule_path))
  geo_info <- S2_extract_geoinfo(xml_file, resolution)
  
  jp2 <- list.files(path = granule_path,
                    pattern = paste0(band, '_.*', resolution, 'm.jp2$'),
                    full.names = TRUE, recursive = TRUE)
  
  # # extract observation date and time from granule_path
  # datetime <- as.POSIXct(strptime(sub('.*S2.*_V([0-9]{8}T[0-9]{6})_.*', '\\1', granule_path), '%Y%m%dT%H%M%S', 'UTC'))
  # extract granule ID from granule_path
  granuleID <- sub('.*_(T[0-9]{2}[A-Z]{3})_.*', '\\1', granule_path)
  
  if (length(jp2) == 0)
    stop(paste('Image file (jp2) not found: Band', band, 'Resolution', resolution, 'Granule', granuleID))
  
  # insert granuleID in filename
  mod_filename <- paste0(file_path_sans_ext(filename), '_', granuleID, '_', band, '_', resolution, 'm.', file_ext(filename))
  
  if (file.exists(mod_filename) & overwrite | !file.exists(mod_filename)) {
    # target extent in GDAL conform vector c(ulx,uly,lrx,lry)
    te <- c(geo_info$UL_corner$ULX,
            geo_info$UL_corner$ULY,
            geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
            geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS)
    # assign CRS and extent
    gdal_translate(src_dataset = jp2, 
                   dst_dataset = mod_filename, 
                   a_srs = paste0('+init=', tolower(geo_info$EPSG)),
                   a_ullr = te,
                   output_Raster = TRUE, ...)
    
  } else {
    raster(mod_filename)
  }
}