#' API Hub query
#' 
#' Query the API Hub for Sentinel scenes
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
#' @export
#' 
#' @import XML

Hub_query <- function(uri_query, username, password, xml_file = tempfile(fileext = '.xml'), show_status = FALSE) {
  
  # test if wget is installed
  # if ()
  
  inp_file <- tempfile(fileext = '.inp')
  
  # write url command to file (on Windows: to avoid problems with cmd)
  url_base <- 'https://scihub.copernicus.eu/apihub/search?q='
  url <- paste0(url_base, uri_query)
  writeLines(text = url, con = inp_file)
    
  extra <- paste('--no-check-certificate', 
                 '--user', username,
                 '--password', password,
                 '-O', xml_file,
                 '-i', inp_file)
  
  sys_call <- paste('wget', extra)
  system(sys_call, show.output.on.console = show_status)
  
  xml_file
}