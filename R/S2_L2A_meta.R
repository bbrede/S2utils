#' Extract S2 L2A meta data
#' 
#' Extract S2 L2A meta data
#' 
#' @param S2_safe Chr. S2 SAFE folder ("S2A_USER_PRD_MSIL2A_PDMC_....SAFE")
#' 
#' @return list of granules of list of names elements
#' \describe{
#'  \item{Granule_Name}{Full granule name, e.g. "S2A_USER_MSI_L2A_TL_SGS__20160119T144513_A003008_T31UFT_N02.01"}
#'  \item{Image_names}{Chr[]. Typically JP2000 files}
#'  \item{EPSG}{EPSG code of coordinate reference system, e.g. "epsg:32631"}
#'  \item{Geoinfo}{Data.frame with number rows/columns and upper left corner coordinates}
#'  \item{Sensing_Time}{POSIXct. Granule sensing time in UTC.}
#' }
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import xml2

S2_L2A_meta <- function(S2_safe) {
  
  #### main xml ####
  
  main_xml <- list.files(S2_safe, pattern = 'S2A.*xml$', full.names = TRUE)
  
  if (length(main_xml) != 1)
    stop('Product xml (top level in SAFE folder) not identified!')
  
  main_tree <- read_xml(main_xml)
  
  # list of xml nodes
  granule_nodes <- xml_find_all(main_tree, '//Granules')
  # names, e.g. "S2A_USER_MSI_L2A_TL_SGS__20160119T144513_A003008_T31UGU_N02.01" 
  granule_names <- unname(sapply(granule_nodes, function(node) xml_attrs(node)['granuleIdentifier']))
  # IDs, e.g. "31UGU"
  granule_Ids <- unname(sub('.*_(T[0-9]{2}[A-Z]{3})_.*', '\\1', granule_names))
  # image names (without suffix)
  image_names <- lapply(granule_nodes, function(node) sapply(xml_children(node), xml_text))
  
  
  #### granule xmls ####
  
  # paths to granule xml files
  granule_xmls <- paste0(S2_safe, '/GRANULE/', granule_names, '/', 
                         sub('(.*)MSI(.*)_N[0-9]{2}.[0-9]{2}', '\\1MTD\\2', granule_names), 
                         '.xml')
  granule_xml_exists <- file.exists(granule_xmls)
  if (any(!granule_xml_exists))
    warning('Some granule xml missing: ', granule_xmls[!granule_xml_exists])
  
  granule_trees <- lapply(granule_xmls[granule_xml_exists], read_xml)

  # EPSG codes
  granule_epsg <- tolower(sapply(granule_trees, function(node) xml_text(xml_find_first(node, '//HORIZONTAL_CS_CODE'))))
  
  xml_extract <- function(node, xpath) {
    xml_text(xml_find_first(node, xpath))
  }
  
  # granule geopositions as list(data.frame)
  granule_geoinfo <- lapply(granule_trees, function(node) {
    do.call(rbind, lapply(c(10, 20, 60), function(resolution) {
      
      sizes <- do.call(rbind, lapply(c('NROWS', 'NCOLS'), function(rowcol) {
        # image dimensions in pixels
        as.numeric(xml_extract(node, paste0('//Size[@resolution=', resolution, ']/', rowcol)))
      }))
      
      ul <- do.call(rbind, lapply(c('ULX', 'ULY'), function(corner) {
        # image dimensions in pixels
        as.numeric(xml_extract(node, paste0('//Geoposition[@resolution=', resolution, ']/', corner)))
      }))
      
      data.frame(Resolution = resolution, NROWS = sizes[1], NCOLS = sizes[2], ULX = ul[1], ULY = ul[2])
    }))
  })
  
  sensing_times <- lapply(granule_trees, function(node) {
    # remove decimal seconds
    t <- sub('(.*)[.].*', '\\1', xml_extract(node, '//SENSING_TIME'))
    as.POSIXct(strptime(t, '%FT%T', tz = 'UTC'))
  })
  
  
  #### build output ####
  
  # build together output: list of granules of list of named elements
  out <- lapply(1:length(granule_trees), function(i) {
    list(Granule_Name = granule_names[granule_xml_exists][i],
         Image_names = image_names[granule_xml_exists][[i]],
         EPSG = granule_epsg[i],
         Geoinfo = granule_geoinfo[[i]],
         Sensing_Time = sensing_times[[i]])
  })
  names(out) <- granule_Ids[granule_xml_exists]
  
  out
}