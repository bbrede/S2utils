#' Extract S2 L2A meta data
#' 
#' Extract S2 L2A meta data
#' 
#' @param S2_safe Chr. S2 SAFE folder ("S2A_USER_PRD_MSIL2A_PDMC_....SAFE")
#' 
#' @return list of granules of list of names elements
#' \describe{
#'  \item{Granule_Name}{Full granule name, e.g. "S2A_USER_MSI_L2A_TL_SGS__20160119T144513_A003008_T31UFT_N02.01"}
#'  \item{Image_names}{Data.frame(File[Chr], Resolution[Num], Band[Chr])}
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
  
  #### file listing ####
  
  # directories for granules
  granule_dirs <- list.dirs(file.path(S2_safe, 'GRANULE'), recursive = FALSE)
  # names, e.g. "S2A_USER_MSI_L2A_TL_SGS__20160119T144513_A003008_T31UGU_N02.01" 
  granule_names <- basename(granule_dirs)
  # IDs, e.g. "31UGU"
  granule_Ids <- unname(sub('.*_(T[0-9]{2}[A-Z]{3})_.*', '\\1', granule_names))
  
  images <- lapply(granule_dirs, function(g_dir) {
    # allow only bands that are listed in S2_bands()
    allowed_bands <- paste0('.*_(', paste(S2_bands()$Name, collapse = '|'), ')_.*')
    
    all_jp2 <- list.files(g_dir, pattern = 'S2A.*_.0m.jp2$', recursive = TRUE)
    jp2 <- grep(allowed_bands, all_jp2, value = TRUE)
    res <- as.numeric(sub('.*_(.0)m.jp2$', '\\1', jp2))
    band <- sub(allowed_bands, '\\1', jp2)
    
    data.frame(File = as.character(jp2), Resolution = res, Band = band)
  })
  
  #### granule xmls ####
  
  # paths to granule xml files
  granule_xmls <- unname(sapply(granule_dirs, list.files, pattern = 'S2A.*xml$', full.names = TRUE))

  granule_trees <- lapply(granule_xmls, read_xml)

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
    list(Granule_Name = granule_names[i],
         Image_names = images[[i]],
         EPSG = granule_epsg[i],
         Geoinfo = granule_geoinfo[[i]],
         Sensing_Time = sensing_times[[i]])
  })
  names(out) <- granule_Ids
  
  out
}