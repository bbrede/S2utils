#' Extract geo-location and CRS infos from S2 xml
#' 
#' Extract geo-location and CRS infos from S2 xml
#' 
#' @param S2_xml S2 xml file
#' @param resolution For which resolution shall the information be extracted?
#' 
#' @return list with dimensions (numeric of 2), UL corner (UTM) cooridnates (numeric of 2), EPSG number of CRS
#' 
#' @export
#' 
#' @import XML


# author: Benjamin Brede
# date: 2016-01-16


S2_extract_geoinfo <- function(S2_xml, resolution=c(10, 20, 60)) {
  
  library(XML)
  
  xml_single_value <- function(xpath) 
    lapply(xmlToList(xpathApply(S2_xml, xpath)[[1]]), as.numeric)
  
  # find nodes where node with name Size/Geoposition is parent and attribute resolution has value resolution 
  dims <- sapply(c('NROWS', 'NCOLS'), 
                 function(dim) xml_single_value(paste0('//Size[@resolution=', resolution, ']/', dim)))
  UL_corner <- sapply(c('ULX', 'ULY'), 
                      function(corner) xml_single_value(paste0('//Geoposition[@resolution=', resolution, ']/', corner)))
  # find node with name HORIZONTAL_CS_CODE
  epsg_node <- xpathApply(xml_tree, '//HORIZONTAL_CS_CODE')
  
  list(dimensions = dims, UL_corner = UL_corner, EPSG = xmlValue(epsg_node[[1]]))
}