#' Translate S2 L2A products
#' 
#' Translate S2 L2A products (underlying jp2000) into other (more common) raster format with CRS.
#' 
#' @param S2_folder Folder that contains the S2 L2A product (typically suffixed with .SAFE)
#' @param out_folder Directory to write the outputs to
#' @param band Band to extract, can be spectral (B01 to B12 plus B8A) or thematic (SCL or CLD)
#' @param resolution Band resolution in m
#' @param granule Which granules to extract (regex is allowed)
#' @param skipExisting Shall files that have already been writen be ignored?
#' 
#' @return list of RasterLayers corresponding to the granules
#' 
#' @export
#' 
#' @import raster
#' @import gdalUtils
#' @import rgdal


# author: Benjamin Brede
# date: 2016-01-16


S2_L2A_translate <- function(S2_folder, out_folder, band, resolution=c(10, 20, 60), granule=NULL, skipExisting=TRUE) {
  
  library(raster)
  library(gdalUtils)
  library(rgdal)
  # TODO: require gdal version >= 2.1 -> utils::compareVersion 
  
  # list all granule folders
  all_granules <- list.dirs(file.path(S2_folder, 'GRANULE'), full.names = FALSE, recursive = FALSE)
  if (is.null(granule))
    granules <- all_granules
  else
    granules <- grep(granule, all_granules, value = TRUE)  
  
  out_list <- lapply(granules, function(g) {
    
    # granule id (letter/number combination before spectral band in filename)
    g_id <- gsub('.*_([A-Z][0-9]{2}[A-Z]{3})_.*', '\\1', g)
    datetime <- gsub('.*_([0-9]{8}T[0-9]{6})_.*', '\\1', g)
    g_folder <- file.path(S2_folder, 'GRANULE', g)
    geo_info <- S2_extract_geoinfo(list.files(g_folder, 'xml$', full.names = TRUE), resolution)
    
    jp2 <- list.files(path = g_folder,, 
                      pattern = paste0(band, '_.*', resolution, 'm.jp2'),
                      full.names = TRUE, recursive = TRUE)
    file_id <- paste('S2_L2A', datetime, g_id, band, paste0(resolution, 'm'), sep = '_')
    # temporary file for storing from gdal_translate
    tmp <- tempfile(fileext = '.tif') #file.path(tmpdir, paste0(file_id, '.tif'))
    # destination file
    dst <- file.path(out_folder, paste0(file_id, '.tif'))
    
    if (!file.exists(dst) | !skipExisting) {
      r <- gdal_translate(src_dataset = jp2, dst_dataset = tmp, output_Raster = TRUE)
      
      # assign CRS and extent
      crs(r) <- CRS(paste0('+init=' ,geo_info$EPSG))
      extent(r) <- extent(c(geo_info$UL_corner$ULX,
                            geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
                            geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
                            geo_info$UL_corner$ULY))      
      # data
      dt <- ifelse(band %in% c('SCL', 'CLD'), 'INT1U', 'INT2U')
      
      writeRaster(r, filename = dst, datatype = dt, overwrite = TRUE)
      
      ### GDALWARP - did not work
      # target extent in UTM
      #te <- c(geo_info$UL_corner$ULX, 
      #        geo_info$UL_corner$ULY - resolution * geo_info$dimensions$NROWS,
      #        geo_info$UL_corner$ULX + resolution * geo_info$dimensions$NCOLS,
      #        geo_info$UL_corner$ULY)
      #gdalwarp(jp2, dst, t_srs = geo_info$EPSG, te = te)
    } else
      raster(dst)
  })
  
  names(out_list) <- granules
  out_list
}