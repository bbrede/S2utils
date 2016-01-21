# author: Benjamin Brede
# date: 2016-01-16


# translate S2 jp2000 (w/o CRS) to specified other format (projection = native S2 = UTM)
# use rasterOptions() to set out format
S2_L2A_translate <- function(S2_folder, band, resolution=c(10, 20, 60), out_folder, skipExisting=TRUE) {
  
  require(raster)
  require(gdalUtils)
  require(rgdal)
  # TODO: require gdal version >= 2.1
  
  # list granule folders, exclude first (GRANULE folder itself, listed by list.dirs function)
  granules <- list.dirs(file.path(S2_folder, 'GRANULE'), full.names = FALSE, recursive = FALSE)
  
  lapply(granules, function(g) {
    
    # granule id (letter/number combination before spectral band in filename)
    g_id <- gsub('.*_([A-Z][0-9]{2}[A-Z]{3})_.*', '\\1', g)
    datetime <- gsub('.*_([0-9]{8}T[0-9]{6})_.*', '\\1', g)
    g_folder <- file.path(S2_folder, 'GRANULE', g)
    geo_info <- S2_L2A_extract_geolocations(list.files(g_folder, 'xml$', full.names = TRUE), resolution)
    
    jp2 <- list.files(path = g_folder,#file.path(g_folder, 'IMG_DATA'), 
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
}