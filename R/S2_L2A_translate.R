#' Translate S2 L2A products
#' 
#' Translate an S2 L2A scene into other raster format.
#' 
#' @param S2_safe Chr. S2 SAFE folder (S2A_USER_PRD_MSIL2A_PDMC_....SAFE)
#' @param band Chr. Band to extract, see \code{\link{S2_bands}}.
#' @param resolution Num. Resolution in m, allowed: 10, 20, 60.
#' @param granules Chr. Granules to translate, prefixes with 'T' (e.g. 'T32ULD'), 'all' for all granules in the product (see \link{https://sentinel.esa.int/documents/247904/1955685/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml})
#' @param filename Chr. Output filename; will be automatically suffixed with granuleID, band name and resolution
#' @param sep Chr. Seperator for suffixed filenames
#' @param ... Additional arguments as for \code{\link{gdal_translate}}, not allowed: a_srs, a_ullr.
#' 
#' @return Named list of Chr[], containing the filenames. Names are granule identifiers.
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
#' # use co argument to access creation options for GTiff files as in gdal_translate
#' S2_L2A_translate(S2, band = 'B8A', resolution = 20, granules = 'T31UFU', 'test.tif', co = c('COMPRESS=LZW'))  

S2_L2A_translate <- function(S2_safe, band, resolution = c(10, 20, 60), granules = 'all', filename, sep = '_', ...) {
      
  s2_meta <- S2_L2A_meta(S2_safe)
  
  available_granules <- names(s2_meta)
  
  # granules to process
  if (granules[1] == 'all') {
    proc_granules <- available_granules
  } else {
    proc_granules <- available_granules[available_granules %in% granules]
  }
  
  # process per granule
  out <- lapply(proc_granules, function(g) {
    
    granule_meta <- s2_meta[[g]]
    # granule folder
    granule_path <- file.path(S2_safe, 'GRANULE', granule_meta$Granule_Name)
    
    if (!dir.exists(granule_path))
      stop('Granule folder does not exists: ', granule_path)
    
    out_list <- per_band <- lapply(band, function(b) {
      per_resolution <- lapply(resolution, function(r) {
        
        # search pattern
        img_pattern <- paste0(grep(paste0(b, '_.*', r, 'm$'), granule_meta$Image_names, value = TRUE), '.jp2$')
        img_file <- list.files(granule_path, img_pattern, recursive = TRUE, full.names = TRUE)
        
        if (length(img_file) != 1)
          stop('Image search had too many results! Probably no images available for Band: ', b, ', resolution: ', r)
        
        mod_filename <- paste0(file_path_sans_ext(filename), sep, g, sep, b, sep, r, 'm.', file_ext(filename))
        
        # target extent in GDAL conform vector c(ulx,uly,lrx,lry)
        geoinfo <- subset(granule_meta$Geoinfo, Resolution == r)
        
        te <- c(geoinfo$ULX,
                geoinfo$ULY,
                geoinfo$ULX + r * geoinfo$NCOLS,
                geoinfo$ULY - r * geoinfo$NROWS)
        
        tryCatch({
          # assign CRS and extent
          gdal_translate(src_dataset = img_file, 
                         dst_dataset = mod_filename, 
                         a_srs = paste0('+init=', granule_meta$EPSG),
                         a_ullr = te,
                         ...)
        }, 
        error = function(e) stop('GDAL could not translate file:', img_file))
        
        mod_filename
      })
    })
    
    unlist(out_list)
  })
  
  # assign granule names to list elements
  names(out) <- proc_granules
  out
}