#' Extract sensor geometry
#' 
#' Extract sensor geometry
#' 
#' @param S2_xml S2 granule xml file
#' @param mode Mean values or whole raster for the granule?
#' @param ... Additional arguments as for \code{\link{writeRaster}}
#' 
#' @return Named list with mean values (for mode 'mean') or SpatialPointsDataFrame (for mode 'spatial')
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import XML
#' @import sp

S2_extract_geometry <- function(S2_xml, mode = c('mean', 'spatial'), ...) {
  
  xml_tree <- xmlParse(S2_xml)
  # relative angle between two angles
  rel_angle <- function(a, b) abs((a - b + 180) %% 360 - 180)
  
  # get single values from XML nodes
  getVals <- function(xpath) {
    as.numeric(sapply(xpathApply(xml_tree, xpath), function(n) xmlValue(n)))
  }
  
  # extract grid of values (coerced to numeric vector) from XML nodes
  # xpath only allowed to aim at one grid at a time
  getGrid <- function(xpath) {
    
    nodes <- xpathApply(xml_tree, xpath)
    
    as.numeric(sapply(nodes, function(n) {
      x <- unlist(strsplit(xmlValue(n), ' '))
      x[x == 'NaN'] <- NA
      as.numeric(x)
    }))
  }
  
  if (mode == 'mean') {

    theta_sun <- getVals('//Mean_Sun_Angle/ZENITH_ANGLE')
    theta_view <- mean(getVals('//Mean_Viewing_Incidence_Angle/ZENITH_ANGLE'))
    
    phi_sun <- getVals('//Mean_Sun_Angle/AZIMUTH_ANGLE')
    # phi given for each band separately
    phi_view <- mean(getVals('//Mean_Viewing_Incidence_Angle/AZIMUTH_ANGLE'))
    # relative azimuth
    phi_rel <- rel_angle(phi_sun, phi_view)
    
    list(Theta_sun = theta_sun, Theta_view = theta_view, Phi_sun = phi_sun, Phi_view = phi_view, Phi = phi_rel)
    
  } else if (mode == 'spatial') {
    
    # resolution at which to extract the geo tags; only necessary to calculate raster extent
    res <- 10
    geo_info <- S2_extract_geoinfo(S2_xml, res)
    
    theta_sun <- getGrid('//Sun_Angles_Grid/Zenith/Values_List/VALUES')
    phi_sun <- getGrid('//Sun_Angles_Grid/Azimuth/Values_List/VALUES')
    
    # all detector IDs
    detectorIds <- unique(as.numeric(sapply(xpathApply(xml_tree, '//Viewing_Incidence_Angles_Grids'), xmlAttrs)['detectorId',]))
    
    # lsit with matrixes of zenith and azimuth angles
    view_grids <- lapply(c('Zenith', 'Azimuth'), function(angle) {
      # viewing geometries for each band differently, bandIds in xml-meta files from 0 to 12
      do.call(cbind, lapply(0:12, function(band) {
        # geometries provided per detector (per band); no overlap of information between detectors/only one detector grid holds the geometry info
        detector_grids <- lapply(detectorIds, function(id) {
          grid <- getGrid(xpath = paste0("//Viewing_Incidence_Angles_Grids[@bandId='", band, "'and @detectorId='", id, "']/", angle, "/Values_List/VALUES"))
        })
        # one col in this matrix represents one detector matrix (flattend to a vector)
        lined <- do.call(cbind, detector_grids)
        # remove NAs = take only the value from valid detectors
        apply(lined, 1, mean, na.rm = TRUE)
      }))
    })
    
    theta_view <- view_grids[[1]]
    # calc relative azimuth between view and sun
    rel_phi <- apply(view_grids[[2]], 2, function(x) rel_angle(x, phi_sun))
    
    # prepare data for SPDF
    data <- data.frame(cbind(theta_sun, theta_view, rel_phi))
    names(data) <- c('Theta_sun', 
                     paste0('Theta_view_', c(paste0('B0', 1:8), 'B8A', 'B09', paste0('B', 10:12))),
                     paste0('Phi_rel_', c(paste0('B0', 1:8), 'B8A', 'B09', paste0('B', 10:12))))
    
    x <- rep(seq(geo_info$UL_corner$ULX, geo_info$UL_corner$ULX + res * geo_info$dimensions$NCOLS, length.out = ncol(m)), nrow(m))
    y <- rep(seq(geo_info$UL_corner$ULY, geo_info$UL_corner$ULY - res * geo_info$dimensions$NROWS, length.out = nrow(m)), each = ncol(m))
    
    pts <- SpatialPoints(cbind(x, y), proj4string = CRS(paste0('+init=', tolower(geo_info$EPSG))))
    SpatialPointsDataFrame(pts, data)
  } 
}