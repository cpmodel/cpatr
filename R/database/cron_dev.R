library(dplyr)
library(lubridate)
library(RPostgres)

  print('-- Backing-up Database ----')

  path_one_drive <- "C://Users//aavan//Dropbox//backup"

  time_s <-  with_tz(Sys.time(), tzone = "America/New_York") %>%
    as.character() %>%
    gsub(' ','_', .) %>%
    gsub('-','_',.) %>%
    gsub(':','_',.)

  time_s

  file_save <- paste0(path_one_drive,'//', time_s, '.rds')

load('creds.Rdata')

con <- dbConnect(RPostgres::Postgres(),
                 dbname =  creds$dbname,
                 host =  creds$host,
                 port = creds$port,
                 user = creds$user,
                 password = creds$password)

table_list <- dbListTables(con) %>% sort()

print(table_list)


output_rds <- table_list %>%
  purrr::map(~ collect(tbl(con, .x))) %>%
  purrr::set_names(table_list)

# output_rds <- list()
# for(t in 1:length(table_list)){
#   print(t)
#   tt <- tbl(con, table_list[t]) %>% collect()
#   output_rds[[table_list[t]]] = tt
# }
saveRDS(output_rds, file_save, compress = T)


