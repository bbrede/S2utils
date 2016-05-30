#' Extract sensor geometry
#' 
#' Extract sensor geometry
#' 
#' @param S2_xml S2 granule xml file
#' @param mode Mean values or whole raster for the granule?
#' @param ... Additional arguments as for \code{\link{writeRaster}}
#' 
#' @return Named list (for mode 'mean') or RasterBrickStack (for mode 'raster')
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import XML
#' @import raster

S2_extract_geometry <- function(S2_xml, mode = c('mean', 'raster'), ...) {
  
  library(XML)
  
  xml_tree <- xmlParse(S2_xml)
  # relative angle between two angles
  rel_angle <- function(a, b) abs((a - b + 180) %% 360 - 180)
  
  # get single values from nodes
  getVals <- function(xpath) {
    as.numeric(sapply(xpathApply(xml_tree, xpath), function(n) xmlValue(n)))
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
    
  } else if (mode == 'raster') {
    
    spacing_col <- getVals('//COL_STEP')
    spacing_row <- getVals('//ROW_STEP')
    
    if (length(unique(spacing_col)) != 1 | length(unique(spacing_row)) != 1)
      stop('Viewing grids do not have same spacing!')
    
    spacing <- c(unique(spacing_col), unique(spacing_row))
    
    # extract grid of values
    # xpath only allowed to aim at one grid at a time
    getGrid <- function(xpath) {
      nodes <- xpathApply(xml_tree, xpath)
      t(sapply(nodes, function(n) {
        x <- unlist(strsplit(xmlValue(n), ' '))
        x[x == 'NaN'] <- NA
        as.numeric(x)
      }))
    }
    
    theta_sun <- getGrid('//Sun_Angles_Grid/Zenith/Values_List/VALUES')
    phi_sun <- getGrid('//Sun_Angles_Grid/Azimuth/Values_List/VALUES')
    
    # all detector IDs
    detectorIds <- unique(as.numeric(sapply(xpathApply(xml_tree, '//Viewing_Incidence_Angles_Grids'), xmlAttrs)['detectorId',]))
    
    # viewing geometries for each band differently; bandIds in xml-meta files from 0 to 12
    theta_view <- lapply(0:12, function(b) {
      detector_grids <- lapply(detectorIds, function(id) {
        grid <- getGrid(xpath = paste0("//Viewing_Incidence_Angles_Grids[@bandId='", b, "'and @detectorId='", id, "']/Zenith/Values_List/VALUES"))
      })
      # one line in this matrix represents one detector matrix
      lined <- do.call(rbind, lapply(detector_grids, as.numeric))
      # remove NAs by substituting them with values from other detectors, then return into matrix again
      matrix(apply(lined, 2, mean, na.rm = TRUE), nrow = nrow(detector_grids[[1]]), ncol = ncol(detector_grids[[1]]))
    })
        
    theta_view_stack <- stack(lapply(theta_view, raster), ...)
    names(theta_view_stack) <- c(paste0('B0', 1:8), 'B8A', 'B09', paste0('B', 10:12))
    theta_view_stack
  } 
}