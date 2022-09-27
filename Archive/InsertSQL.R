#' #' Insert Table to SQL
#' #'
#' #' @param con SQL Connection Object
#' #' @param data Data in tibble or data.frame format
#' #' @param TableID Only Lowercase Letters, Numbers, and Underscores
#' #' @param TableName Full Name of Table
#' #' @param TableCategory Category or Project Name
#' #' @param Date Date R object, as.Date('yyyy-mm-dd')
#' #' @param Comments ...
#' #'
#' #' @return
#' #' @import tidyr dplyr purrr RPostgres DBI
#' #'
#' #' @examples
#' #' @export
#' insert_table <- function(con,
#'                            data,
#'                            TableID,
#'                            TableName,
#'                            TableCategory,
#'                            Date,
#'                            Comments){
#'
#'   meta <- tibble(
#'     TableID = TableID,
#'     TableName = TableName,
#'     TableCategory = TableCategory,
#'     Date = Date,
#'     Comments = Comments
#'   )
#'
#'   dbWriteTable(con, TableID, data, overwrite = T)
#'   dbWriteTable(con, 'meta_data', meta, append = T)
#' }
