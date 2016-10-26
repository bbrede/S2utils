#' Extract geo-location and CRS infos from S2 xml
#' 
#' Extract geo-location and CRS infos from S2 xml
#' 
#' @param S2_xml S2 product xml file
#' @param resolution For which resolution shall the information be extracted?
#' 
#' @return list with dimensions (numeric of 2), UL corner (UTM) coordinates (numeric of 2), EPSG number of CRS
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import XML

S2_extract_geoinfo <- function(S2_xml, resolution=c(10, 20, 60)) {
  
  xml_tree <- xmlParse(S2_xml)
  
  xml_single_value <- function(xpath) 
    lapply(xmlToList(xpathApply(xml_tree, xpath)[[1]]), as.numeric)
  
  # find nodes where node with name Size/Geoposition is parent and attribute resolution has value resolution 
  dims <- sapply(c('NROWS', 'NCOLS'), 
                 function(dim) xml_single_value(paste0('//Size[@resolution=', resolution, ']/', dim)))
  UL_corner <- sapply(c('ULX', 'ULY'), 
                      function(corner) xml_single_value(paste0('//Geoposition[@resolution=', resolution, ']/', corner)))
  # find node with name HORIZONTAL_CS_CODE and extract EPSG code
  epsg_node <- xpathApply(xml_tree, '//HORIZONTAL_CS_CODE')
  epsg <- xmlValue(epsg_node[[1]])
  
  list(dimensions = dims, UL_corner = UL_corner, EPSG = epsg)
}