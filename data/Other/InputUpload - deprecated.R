
rm(list = ls(),envir = .GlobalEnv)

library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

library(RPostgres)

library(cpatr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load('../calibration/creds.Rdata')

con <- dbConnect(RPostgres::Postgres(),
                 dbname =  creds$dbname,
                 host =  creds$host,
                 port = creds$port,
                 user = creds$user,
                 password = creds$password)

# List of existing databases
dbListTables(con) %>% sort()


# Energy Consumption:
ec_hist     <- read.csv('EnergyConsumption/EnergyConsumption_1990_to_2019.csv') %>%
                # Dropping initial counting column
                select(-X) %>%
                pivot_longer(cols = 'ele':'wav', names_to = 'sectorcode', values_to = 'value') %>%
                # Column names should have no capital letters
                rename('countrycode' = country_code,
                       'fueltype' = FuelType) %>%
                select(countrycode, sectorcode, fueltype, year, value)


dbWriteTable(con, name  = 'energyconsumption',
                  value = ec_hist, overwrite = T)



cpatr::insert_table(con,
                    data = prices_expanded,
                    TableID = 'dom_prices_hist_expanded', #keep lowercase without fancy symbols
                    TableName = 'Domestic prices and price components, historic data, sector groups expanded into sectors.',
                    TableCategory = 'Input Data',
                    Date = Sys.Date(),
                    Comments = 'Latest available historic domestic prices dataset, expanding group sectors into sectors')


dbWriteTable(con, name  = 'dom_prices_hist_expanded',
                  value = dom_prices_hist_expanded, overwrite = T)


prices_fetch  <- cpatr::fetch_table(con, TableID = 'dom_prices_hist_expanded')





readRDS('../data/Prices/2_Processed/Expand_prices_for_cpat_all_years_2022_04_15')



