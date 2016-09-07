#' API Hub query
#' 
#' Query the API Hub for Sentinel scenes
#' 
#' @description Performs Hub queries via wget.
#' 
#' @param uri_query URI query as used in the Sentinel SciHub
#' @param username API Hub username
#' @param password API Hub password
#' @param xml_file XML file where query results shall be written to
#' @param show_status Show (status) messages of the wget download process
#' 
#' @return XML file name (same as input)
#' 
#' @references \link{https://scihub.copernicus.eu/twiki/do/view/SciHubUserGuide/5APIsAndBatchScripting#Query_via_Wget}
#' 
#' @author Benjamin Brede
#' 
#' @export
#' 
#' @import XML

Hub_query <- function(uri_query, username, password, xml_file = tempfile(fileext = '.xml'), show_status = FALSE) {
  
  uuid_query <- paste0('https://scihub.copernicus.eu/apihub/search?q=', uri_query)
  
  cmd <- paste('wget --no-check-certificate', 
               paste0('--user=', username),
               paste0('--password=', password),
               paste0('--output-document=', shQuote(xml_file)),
               shQuote(uuid_query))
  system(cmd, show.output.on.console = show_status)
  
  xml_file
}