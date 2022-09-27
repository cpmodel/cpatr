library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)

namesp <- c('A', 'B')

params <- tibble(
  ParamOne = c('A', 'B'),
  ParamTwo = c(3, 5),
  Titles = c('Energy', 'Coal'),
  yvar = c('vs', 'hp')
)

params <- asplit(params, 1)

datax <- mtcars %>% as_tibble()
datax$name = row.names(mtcars)

ff <- function(data, varf){
  data %>%
    filter(name == varf)
}

ff(datax, 'Mazda RX4')

## option 2 > function

myfun <- function(x, data){
  p1 <- x[1]
  p2 <- x[2]
  p3 <- x[3]
  var <- x[4]
  ## code...

  # output <- data %>%
  #   mutate(newcol = paste(p1, p2))
  output <- data %>%
    ggplot(aes(x = mpg, y = !!sym(var))) +
    geom_point() +
    ggtitle(p3)

  return(output)
}


res <- params %>%
  map(~myfun(x = .x, data = datax))

names(res) = namesp

save(res, file = 'res.Rdata')
