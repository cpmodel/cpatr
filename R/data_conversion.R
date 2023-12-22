library(cpatr)
library(tidyverse)
library(purrr)

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


FullBaseList = cpatr::BaseList_AllCountries

for (element in names(FullBaseList)) {
  if (!'list' %in% class(FullBaseList[[element]])){
    if ('character' %in% class(FullBaseList[[element]]) |
        'numeric' %in% class(FullBaseList[[element]])){
      write.csv(FullBaseList[element], file = paste0(element, ".csv"),
                row.names = FALSE)
    } else {
      matrix_columns = sub(".*\\.", "", colnames(as.data.frame(FullBaseList[element])))
      temp <- as.data.frame(FullBaseList[element])
      colnames(temp) <- matrix_columns
      write.csv(temp, file = paste0(element, ".csv"),
                row.names = FALSE)
    }
  } else {
    for (LU_element in names(FullBaseList[[element]])){
        print(LU_element)
        print(element)
        matrix_columns = sub(".*\\.", "", colnames(as.data.frame(FullBaseList[[element]][LU_element])))
        temp <- as.data.frame(FullBaseList[[element]][LU_element])
        colnames(temp) <- matrix_columns
        write.csv(temp, file = paste0(element, "_", LU_element, ".csv"),
                  row.names = FALSE)
    }
  }
}
