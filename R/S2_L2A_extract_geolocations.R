# author: Benjamin Brede
# date: 2016-01-16


S2_L2A_extract_geolocations <- function(S2_xml, resolution=c(10, 20, 60)) {
  
  require(XML)
  
  xml <- xmlParse(S2_xml)
  
  xml_single_value <- function(xpath) 
    lapply(xmlToList(xpathApply(xml, xpath)[[1]]), as.numeric)
  
  # find nodes where node with name Size/Geoposition is parent and attribute resolution has value resolution 
  dims <- sapply(c('NROWS', 'NCOLS'), 
                 function(dim) xml_single_value(paste0('//Size[@resolution=', resolution, ']/', dim)))
  UL_corner <- sapply(c('ULX', 'ULY'), 
                      function(corner) xml_single_value(paste0('//Geoposition[@resolution=', resolution, ']/', corner)))
  # find node with name HORIZONTAL_CS_CODE
  epsg_node <- xpathApply(xml, '//HORIZONTAL_CS_CODE')
  
  list(dimensions = dims, UL_corner = UL_corner, EPSG = xmlValue(epsg_node[[1]]))
}