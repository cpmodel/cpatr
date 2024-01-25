library(cpatr)
library(tidyverse)

HistoricDataset = cpatr::DB_AllCountries
FullBaseList = cpatr::BaseList_AllCountries

# Edit the results pathway accordingly (directing to CPAT_Python repository)
results_loc <- "C:/Users/wb582890/OneDrive - WBG/Documents/github/cpat_python/csv_data/"

if (!file.exists(results_loc)){
  dir.create(results_loc)
}

# Extracting DB_AllCountries
conv_db_loc <- paste0(results_loc, "DB_AllCountries/")

if (!file.exists(conv_db_loc)){
  dir.create(conv_db_loc)
}

placeholder_id <- data.frame(HistoricDataset %>%
                               select(CountryCode, SectorCode, FuelCode))

variables <- colnames(HistoricDataset)[4:length(colnames(HistoricDataset))]

for (i in variables){
  tempdata <- data.frame(HistoricDataset[[i]]) %>%
    set_names(attr(HistoricDataset[[i]], "dimnames")[[2]])

  tempdata <- cbind(placeholder_id, tempdata)
  assign(i, tempdata)

  if (i == "XCT"){
    write.csv(get(i), paste0(conv_db_loc, "XCT_pCO2.csv"),
              row.names=FALSE)
  } else if (i == "XETSP") {
    write.csv(get(i), paste0(conv_db_loc, "XETSP_pCO2.csv"),
              row.names=FALSE)
  } else {
    write.csv(get(i), paste0(conv_db_loc, paste0(i,".csv")),
              row.names=FALSE)
  }
}

# Extracting BaseList_AllCountries
conv_base_loc <- paste0(results_loc, "BaseList_AllCountries/")

if (!file.exists(conv_base_loc)){
  dir.create(conv_base_loc)
}

for (element in names(FullBaseList)) {
  if (!'list' %in% class(FullBaseList[[element]])){
    if ('character' %in% class(FullBaseList[[element]]) |
        'numeric' %in% class(FullBaseList[[element]])){
      print(element)
      matrix_columns = sub(".*\\.", "", colnames(as.data.frame(FullBaseList[element])))
      temp <- as.data.frame(FullBaseList[element])
      colnames(temp) <- matrix_columns
      write.csv(temp, file = paste0(conv_base_loc, paste0(element, ".csv")),
                row.names = FALSE)
    } else {
      print(element)
      write.csv(FullBaseList[element], file = paste0(element, ".csv"),
                row.names = FALSE)
    }
  } else {
    for (LU_element in names(FullBaseList[[element]])){
        print(LU_element)
        # print(element)
        matrix_columns = sub(".*\\.", "", colnames(as.data.frame(FullBaseList[[element]][LU_element])))
        temp <- as.data.frame(FullBaseList[[element]][LU_element])
        colnames(temp) <- matrix_columns
        write.csv(temp, file = paste0(conv_base_loc, paste0(element, "_", LU_element, ".csv")),
                  row.names = FALSE)
    }
  }
}

conv_price_loc <- paste0(results_loc, "Price_AllCountries/")

if (!file.exists(conv_price_loc)){
  dir.create(conv_price_loc)
}

write_csv(RInputs_InternationalPrices_RegionMarket,
          paste0(conv_price_loc, "RegionMarket.csv"))
write_csv(RInputs_InternationalPrices_IntPrices,
          paste0(conv_price_loc, "IntPrices.csv"))
write_csv(RInputs_InternationalPrices_RegionAssumptions,
          paste0(conv_price_loc, "RegionAssumptions.csv"))

conv_gdp_loc <- paste0(results_loc, "GDP_AllCountries/")

if (!file.exists(conv_gdp_loc)){
  dir.create(conv_gdp_loc)
}

write_csv(RInputs_RawGDPRelativeToBase, paste0(conv_gdp_loc, "RawGDPRelativeToBase.csv"))

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
