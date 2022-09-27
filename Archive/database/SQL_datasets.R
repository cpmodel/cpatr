
rm(list = ls(),envir = .GlobalEnv)

library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

library(RPostgres)

library(cpatr)
library(help = cpatr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load('creds.Rdata')

con <- dbConnect(RPostgres::Postgres(),
          dbname =  creds$dbname,
          host =  creds$host,
          port = creds$port,
          user = creds$user,
          password = creds$password)

dbListTables(con) %>% sort()


# token <- 'xyz' # personal git token #load('token.Rdata')
# devtools::install_github('wb-mrio/cpatr',
#                          upgrade = 'never',
#                          dependencies = T,
#                          auth_token = token)


# writing a table ----
data_for_sql <- iris %>%
  as_tibble() %>%
  purrr::set_names(tolower) #don't use upper case in names, important

dbWriteTable(con, name = 'iris_tibble', value = data_for_sql, overwrite = T)
dbListTables(con) %>% sort()

# fetch data -- option one -- fetch all and use dplyr ----
# calculations are done on local computer

# write basic query
data_from_sql <- dbGetQuery(con, "SELECT * FROM iris_tibble") %>%
  as_tibble() %>%
  filter(species == 'setosa')

# fetch data -- option two -- collect data ----
# calculations are done on sql server side (lazy eval)

data_from_sql <- tbl(con, "iris_tibble")

data_from_sql <- tbl(con, "iris_tibble") %>%
  filter(species == 'setosa') %>%
  collect()

# fetch data -- option three -- write own query ----

data_from_sql <- dbGetQuery(con, "SELECT * FROM iris_tibble WHERE species = 'setosa' ") %>%
  as_tibble()

# remove table ----
dbRemoveTable(con, name = 'iris_tibble')
dbListTables(con) %>% sort()



## using wrappers from cpatr ----
# saving table and meta data on second table

df <- tibble(L = LETTERS, N = 1:26)

cpatr::insert_table(con,
             data = df,
             TableID = 'demo_dummmy_df', #keep lowercase without fancy symbols
             TableName = 'Data For Letters And Numbers',
             TableCategory = 'Demo Project',
             Date = Sys.Date(),
             Comments = 'Saving a dummy data with letters and numbers')


result <- cpatr::fetch_table(con, TableID = 'demo_dummmy_df')
# returns a list with two tables, data itself and metadata

result$data
result$meta_data

# saving tibble matrix to SQL ----

tbm <- read_rds('../data/data_input.RDS')
dim(tbm)

cpatr::is_tibble_matrix(iris)

cpatr::is_tibble_matrix(tbm)

# tbm will lose the matrix data if saved to sql as a regular table
# need to use a function to convert into regular table

tbm2 <- cpatr::convert_tibble_matrix_to_tibble(tbm)
dim(tbm2)
names(tbm2)

cpatr::insert_table(con,
                    data = tbm2,
                    TableID = 'energy_table', #keep lowercase without fancy symbols
                    TableName = 'Data Energy Consumption',
                    TableCategory = 'Demo Project',
                    Date = Sys.Date(),
                    Comments = '.')


tibble_data <- cpatr::fetch_table(con, TableID = 'energy_table')
tibble_data$meta_data

tbm_data <- tibble_data$data %>% cpatr::tibble_to_tibble_matrix()
tbm_data$rp














