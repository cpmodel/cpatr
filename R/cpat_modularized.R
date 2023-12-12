# Loading the CPAT-R package
library(cpatr)

# Additional packages required for reporting purposes
library(tidyverse)
library(dplyr)

# FullHistoricalDataset: Takes the historical data in the required format,
# covering all variables for every combination of Country, Sector and Fuel.
# This data comes preloaded into the package under the name of DB_AllCountries.

FullHistoricalDataset = cpatr::DB_AllCountries

# FullBaseList: Requires the templates for the data structures, global definitions,
# lookups, among others. The dataset used as input, can be found as

FullBaseList = cpatr::BaseList_AllCountries
FullBase_sum = sum(FullBaseList$TemplMat) # matrix of all zeroes

# UserScen: The list of inputs that define the policy scenario. There are six elements
# required on this list. The user can retrieve the specifications of both by using
# the BaselineInputs() function:

# Each scenario is a list of parameters. The object below is hence a nested list.
IncludedScenarios     <- cpatr::BaselineInputs(BaseL = BaseList_AllCountries)

# To check the names of the elements that define one scenario in *cpatr*, we can run:
head(IncludedScenarios$Scenario1)

# CountryList: The vector of countries over which to run cpatr. These have to be provided
# in ISO3, with all capital letters. If all countries and regions are needed, the user can input:

# This vector contains more than 180 iso3 country codes.
CountryList = cpatr::BaseList_AllCountries$SelCountry

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

# Reading the built-in scenarios
IncludedScenarios     <- cpatr::BaselineInputs(BaseL = BaseList_AllCountries)

# Running CPAT for all countries, for two scenarios:
TestSingleCountry   <- cpatr::SimpleCPAT(
  FullHistoricDataset = DB_AllCountries,
  FullBaseList        = BaseList_AllCountries,
  # I modified Scenario1 to UserScenario, but nothing changed?
  UserScen            = UserScenario,
  CountryList         = 'USA')

# Scenario1, no option to get UserScenario
Sc1 <- TestSingleCountry$Scenario1

