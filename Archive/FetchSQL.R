#'
#' #' Load Data from SQL
#' #'
#' #' @param con SQL Connection Object
#' #' @param TableID Table ID as saved to SQL DB
#' #'
#' #' @return
#' #' @export
#' #' @import tidyr dplyr purrr RPostgres DBI
#' #'
#' #' @examples
#'
#' fetch_table <-function(con, TableID){
#'
#'   id <- TableID
#'   output <- list()
#'
#'   output$data <- RPostgres::dbGetQuery(con, paste("SELECT * FROM", TableID)) %>% as_tibble()
#'
#'   output$meta_data <- RPostgres::dbGetQuery(con, "SELECT * FROM meta_data") %>% as_tibble() %>%
#'     dplyr::filter(TableID == id)
#'
#'   return(output)
#' }
#'
#'
#'
#'
#'
#'
