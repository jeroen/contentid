## A DBI backed registry

register_dbi <- function(source, con) {
  
  id <- content_id(source)
  
  # https://gist.github.com/jeroen/2087db9eaeac46fc1cd4cb107c7e106b#file-multihash-R
  
  df <- registry_entry(id, source, Sys.time())
  dbi_init(con)
  DBI::dbAppendTable(con, "registry", df)

  id
}


#' @importFrom DBI dbGetQuery dbAppendTable
sources_dbi <- function(id, con) {
  
  
  id <- as_hashuri(id)
  if(is.na(id)){
    warning(paste("id", id, "not recognized as a valid identifier"))
    return( null_query() )
  }
  ## not lazy ops
  sql <- paste0("FROM registry SELECT * WHERE identifier == ", id)
  DBI::dbGetQuery(con, sql)

}


history_dbi <- function(x, con) {

  sql <- paste0("FROM registry SELECT * WHERE source == ", x)
  DBI::dbGetQuery(con, sql)
  
}



## intialize a dbi-based registry
dbi_init <- function(con) {
  
    tables <- DBI::dbListTables(con)
    if("registry" %in% tables) return(NULL)

    r <- registry_entry()
    DBI::dbWriteTable(con, "registry", r[0, ])
  
}



