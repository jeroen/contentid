
#' List all content identifiers that have been seen at a given URL
#' 
#' [query_history] is the complement of [query_sources], in that it filters a table
#' of content identifier : url : date entries by the url. 
#' 
#' @param url A URL for a data file
#' @inheritParams register
#' @param ... additional arguments
#' @return a data frame with all content identifiers that have been seen
#' at a given URL.  If the URL is version-stable, this should be a single 
#' identifier.  Note that if multiple identifiers are listed, older content
#' may no longer be available, though there is a chance it has been registered
#' to a different url and can be resolved with [query_sources].
#' @seealso sources
#' @export
#' @examples
#' \donttest{
#' 
#' query_history("https://zenodo.org/record/3678928/files/vostok.icecore.co2")
#' 
#' }
#'
query_history <- function(url, registries = default_registries(), ...){
  
  ha_out <- NULL
  reg_out <- NULL

  ## Remote host registries  (hash-archive.org type only)
  if (any(grepl("hash-archive.org", registries))){
    remote <- registries[grepl("hash-archive.org", registries)]  
    ha_out <- lapply(remote, function(host) history_ha(url, host = host))
    ha_out <- do.call(rbind, ha_out)
  }
  
  local <- registries[dir.exists(registries)]
  reg_out <- lapply(local, function(dir) history_tsv(url, dir = dir))
  reg_out <- do.call(rbind, reg_out)
  rbind(ha_out, reg_out)
  
}
