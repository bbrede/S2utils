#' API Hub download
#' 
#' Download Sentinel-2 (and 1) data from the API Hub
#' 
#' @description Download using wget and Open Search
#' 
#' @param username API Hub username
#' @param password API Hub password
#' @param xml_file XML file where query results shall be written to
#' @param show_status Show (status) messages of the wget download process
#'  
#' @return Character vector with (successfully) downloaded file names
#' 
#' @references \link{https://scihub.copernicus.eu/twiki/do/view/SciHubUserGuide/5APIsAndBatchScripting#Query_via_Wget}
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import XML

Hub_download <- function(xml_file, target_dir, username, password, overwrite = FALSE, show_status = FALSE) {
  
  # TODO: implement with XML/xpath
  # xmlRoot(xml)
  
  # convert xml to list and search for nodes with name entry
  xml_list <- xmlToList(xmlParse(xml_file))
  entries <- xml_list[names(xml_list) == 'entry']
  
  if (Sys.info()['sysname'] == 'Linux')
  source_uuids <- sapply(entries, function(e) paste0("\u0022https://scihub.copernicus.eu/dhus/odata/v1/Products\u0028'", e$id, "'\u0029/\\$value\u0022"))
  else if (Sys.info()['sysname'] == 'Windows')
    source_uuids <- sapply(entries, function(e) paste0('https://scihub.copernicus.eu/dhus/odata/v1/Products(\'', e$id, '\')/$value'))
  else
    stop('Unsupported OS!')
  
  target_files <- sapply(entries, function(e) paste0(file.path(target_dir, e$title), '.zip'))

  # expand UUIDs and filenames
  expanded <- paste('--no-check-certificate', 
                    '--user', username,
                    '--password', password,
                    '-O', target_files,
                    source_uuids)
  
  # download
  for (i in seq_along(expanded)) {
    if (overwrite | !file.exists(target_files[i]))
      system(paste('wget', expanded[i]))
  }
  
  target_files
}
