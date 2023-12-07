library(cpatr)
library(tidyverse)

# Initializing the list
UserScenario                  = list()

# Parameters defining the carbon price trajectory
UserScenario$CTintroYear      = 2030
UserScenario$CTintroValue     = 20
UserScenario$CTtargetYear     = 2040
UserScenario$CTtargetValue    = 100

# Fuel and sector coverage
UserScenario$SelectedFuels    = c('nga', 'coa', 'oop', 'gso', 'die', 'lpg', 'ker')
UserScenario$SelectedSectors  = c('res', 'foo', 'srv', 'mac')

HistoricDataset = cpatr::DB_AllCountries
FullBaseList = cpatr::BaseList_AllCountries
CleanCountryList = c("USA", "ZAF")
UserScen = UserScenario

if (!file.exists("data_py")){
  dir.create("data_py")
}

placeholder_id <- data.frame(HistoricDataset %>%
                               select(CountryCode, SectorCode, FuelCode))

variables <- colnames(HistoricDataset)[4:length(colnames(HistoricDataset))]

for (i in variables){
  tempdata <- data.frame(HistoricDataset[[i]]) %>%
    set_names(attr(HistoricDataset[[i]], "dimnames")[[2]])

  tempdata <- cbind(placeholder_id, tempdata)
  assign(i, tempdata)

  write.csv(get(i), paste0("data_py/", paste0(i,".csv")),
            row.names=FALSE)
}

