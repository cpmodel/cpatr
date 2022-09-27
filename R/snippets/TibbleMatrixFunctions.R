
#' Verify if tibble contains matrix-columns
#'
#' @description
#' With the `is.tibble.matrix()` function we check every column of the tibble object
#' and detect if any of them contain a matrix-column
#'
#' More documentation on matrix-columns can be found at [https://www.ericrscott.com/post/matrix-columns/](https://www.ericrscott.com/post/matrix-columns/)
#'
#' @param data A tibble object
#'
#' @return An object of class `logical`, TRUE or FALSE
#' @export
#'
#' @examples
#' is.tibble.matrix(mtcars)
#' data("Dmatrix")
#' is.tibble.matrix(Dmatrix)
is.tibble.matrix <- function(data){
  for(cc in 1:ncol(data)){
    if(any(class(data[,cc][[1]]) %in% c('matrix')) |
       any(class(data[,cc]) %in% c('matrix'))){
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Title
#'
#' @param data a tibble-matrix object
#'
#' @import tidyr dplyr purrr stringr
#' @return
#' @export
#'
#' @examples
convert_tibble_matrix_to_tibble <- function(data){
  tibble_new <- c()
  mat_count <- 1 # counting the matrices, code will be flexible
  for(cc in 1:ncol(data)){
    if(any(class(data[,cc][[1]]) %in% c('matrix'))){
      # column is a matrix
      df <- data[,cc][[1]]
      # adding the prefix mat.mat_count. to each colname
      colnames(df) <- paste('matrix', mat_count, names(data[,cc]), colnames(df), sep = '.')
      mat_count <- mat_count + 1
      tibble_new <- tibble_new %>% #binding the matrix
        bind_cols(df)
    }else{ # in case the column is a regular column, not a matrix
      tibble_new <- tibble_new %>% #binding the col
        bind_cols(data[,cc])
    }
  }
  return(tibble_new)
}


#' Title
#'
#' @param data a tibble to convert to tibble-matrix
#'
#' @return
#' @import tidyr dplyr purrr stringr
#' @export
#'
#' @examples
tibble_to_tibble_matrix <- function(data){

  tibble_matrix <- data

  ww <- which(grepl('matrix.', colnames(tibble_matrix))) #find all cols which contain 'matrix.'

  if(length(ww)>0){

    num_matrices <- colnames(tibble_matrix)[ww] %>% # find the numbers of the matrices
      stringr::word(2, sep = '[.]') %>%
      as.numeric() %>%
      unique()

    for(x in num_matrices){
      ww <- which(grepl(paste('matrix',x, sep = '.'), colnames(tibble_matrix)))

      name_mat <- colnames(tibble_matrix)[ww[1]] %>% stringr::word(3, sep = '[.]') #name of the matrix
      matt <- tibble_matrix[,ww] %>% as.matrix()
      colnames(matt) <- colnames(matt) %>% stringr::word(4, sep = '[.]')

      tibble_matrix <- tibble_matrix %>% #append the matrix as a new column
        mutate(!!name_mat := matt %>% as.matrix()) %>%
        relocate(!!name_mat, .before = ww[1]) %>%
        select(-(ww + 1))
    }
  }

  return(tibble_matrix)
}
