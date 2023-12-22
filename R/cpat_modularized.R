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
FullBase_sum = sum(FullBaseList$TemplMat) # GH: Matrix of all zeroes

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

# GH: This SimpleCPAT function is the main function that we need to modularize
# I will add pointers in all functions and subfunctions, check by ctrl + F "# Function "
# Running CPAT for all countries, for two scenarios:
TestSingleCountry   <- cpatr::SimpleCPAT(
  FullHistoricDataset = DB_AllCountries,
  FullBaseList        = BaseList_AllCountries,
  # I modified Scenario1 to UserScenario, but nothing changed?
  UserScen            = UserScenario,
  CountryList         = 'USA')

# Keep only country codes consistent with the existint list of countries
CleanCountryList  <- CountryList[CountryList %in% FullBaseList$SelCountry]

# Function 1: FocusCountry
# Filtered version for the selected countries
# FilteredData      <- FocusCountry function

DD = FullHistoricalDataset
FullBaseL = FullBaseList
SelectedCountryList = c("USA") # CleanCountryList
# GH: should also work for other list e.g., c("USA", "ZAF")
LocalUserScen = UserScenario


# This is the number of unique rows needed for each country, and that will be used for the templates
SC_NumRows        <- (FullBaseL$IDcols %>%
                        select(-CountryCode) %>%
                        distinct() %>%
                        dim(.) )[1]

SelectionNumRows  <- length(SelectedCountryList) * SC_NumRows


# ---------------------------------------- #
# Adjusting BaseList to the new dimensions
# ---------------------------------------- #

# Function 1: FocusCountry (T03-4)
# Filtered version for the selected countries
# FilteredData      <- FocusCountry function

# Initializing the Base List that will be returned to the user
FilterBaseL       <- FullBaseL

# Adjusting the relevant entries of the list
FilterBaseL$IDcols      <- FullBaseL$IDcols %>%
  dplyr::filter( CountryCode %in% SelectedCountryList )
FilterBaseL$SelCountry  <- SelectedCountryList

# Adapting the template matrix for the number of countries selected:
FilterBaseL$TemplMat    <- FullBaseL$TemplMat[1:SelectionNumRows,]

# ------------------------------------------------------- #
# Adjusting the Baseline parameters to the new dimensions #
# ------------------------------------------------------- #

# These depend on country selection, so they need to be adjusted
# Function 1-1 (subfunction) (T01)
# BaselineParams      <- BaselineInputs(BaseL = FilterBaseL)[[1]]

MTinputsDummy               <- list()

# Dummy data created inside the function until having an interface
Scenario1                   <- list()

Scenario1$IntPricesSource   <- 'IMF-WB'
Scenario1$AddExternalityVAT <- FALSE
# Info on new Carbon Tax
Scenario1$CTintroYear       <- 2050
Scenario1$CTintroValue      <- 0
Scenario1$CTtargetYear      <- 2050
Scenario1$CTtargetValue     <- 0
Scenario1$NCTcov_sf         <- FilterBaseL$TemplMat
# Info on new ETS permit
Scenario1$ETSintroYear      <- 2050
Scenario1$ETSintroValue     <- 0
Scenario1$ETStargetYear     <- 2050
Scenario1$ETStargetValue    <- 0
Scenario1$NETScov_sf        <- FilterBaseL$TemplMat
# Info on Existing policies
Scenario1$ApplyExistingCP   <- TRUE
Scenario1$PhaseOut_cs       <- 1-FilterBaseL$TemplMat    # Phaseout consumer subsidy (% of subsidy kept)
Scenario1$PhaseOut_ps       <- 1-FilterBaseL$TemplMat    # Phaseout producer subsidy (% of subsidy kept)
Scenario1$PhaseOut_pc       <- 1-FilterBaseL$TemplMat    # Phaseout price controls (% of price control left)
Scenario1$ShadowPrIncr      <- FilterBaseL$TemplMat      # Shadow price annual growth rate
Scenario1$CovShadowPrice    <- FilterBaseL$TemplMat      # Percentage of shadow price
Scenario1$ExogShockCOVID    <- FilterBaseL$TemplMat      # Exogenous COVID shock

# Grouping the scenarios information in a list
MTinputsDummy$Scenario1     = Scenario1

BaselineParams <- Scenario1

# Baseline policy coverage for new carbon tax: a matrix of zeros whose dimensions depend on the number of countries chosen
# This is used as reference to build the matrix of policy coverage given user's selection of sectors and fuels
BaselineCoverage    <- tibble( FilterBaseL$IDcols %>%
                                 cbind( BaselineParams$NCTcov_sf ))

# ----------------------------------------------- #
# Creating a consistent list of policy parameters #
# ----------------------------------------------- #

# Carbon prices are straightforward to input, but coverage is not.
# The coverage matrix needs to be computed based on the country, fuel and sector selection by the user
NCTcov_sf           <- BaselineCoverage %>%
  pivot_longer(cols = -c(CountryCode,
                         SectorCode,
                         FuelCode),
               names_to = "Year",
               values_to = "Value") %>%
  mutate(Value = if_else(CountryCode %in% SelectedCountryList &
                           SectorCode %in% LocalUserScen$SelectedSectors &
                           FuelCode %in% LocalUserScen$SelectedFuels &
                           Year >= LocalUserScen$CTintroYear,
                         1,
                         0) ) %>%
  pivot_wider(names_from = "Year", values_from = 'Value') %>%
  select(-c(CountryCode, SectorCode, FuelCode)) %>%
  as.matrix()


# With the coverage and the inputs provided by the user, the list of parameters as required by the model can be built
PolicyParams        <- list("IntPricesSource" = BaselineParams$IntPricesSource,
                            "AddExternalityVAT" = BaselineParams$AddExternalityVAT,
                            "CTintroYear" = LocalUserScen$CTintroYear,
                            "CTintroValue" = LocalUserScen$CTintroValue,
                            "CTtargetYear" = LocalUserScen$CTtargetYear,
                            "CTtargetValue" = LocalUserScen$CTtargetValue,
                            "NCTcov_sf" = NCTcov_sf,
                            "ETSintroYear" = BaselineParams$ETSintroYear,
                            "ETSintroValue" = BaselineParams$ETSintroValue,
                            "ETStargetYear" = BaselineParams$ETStargetYear,
                            "ETStargetValue" = BaselineParams$ETStargetValue,
                            "NETScov_sf" = BaselineParams$NETScov_sf,
                            "ApplyExistingCP" = FALSE,
                            "PhaseOut_cs" = BaselineParams$PhaseOut_cs,
                            "PhaseOut_ps" = BaselineParams$PhaseOut_ps,
                            "PhaseOut_pc" = BaselineParams$PhaseOut_pc,
                            "ShadowPrIncr" = BaselineParams$ShadowPrIncr,
                            "CovShadowPrice" = BaselineParams$CovShadowPrice,
                            "ExogShockCOVID" = BaselineParams$ExogShockCOVID)


# ----------------------------------------------- #
# Creating a list of processed elements to return #
# ----------------------------------------------- #

TreatedList         <- list()
TreatedList$BaseL   <- FilterBaseL
TreatedList$BaselineParams  <- BaselineParams
TreatedList$PolicyParams    <- PolicyParams

# Output of TreatedList: FilteredData
FilteredData = TreatedList

# Initializing the Scenarios with filtered data:
MTI               <- list()
MTI$Scenario1     = FilteredData$BaselineParams
MTI$Scenario2     = FilteredData$PolicyParams


#------------------------------------------------------------------#
# Reading the core inputs after filtering for the country selected #
#------------------------------------------------------------------#
HistoricDataset   <- FullHistoricalDataset %>%
  dplyr::filter(CountryCode %in% CleanCountryList)
BaseList          <- FilteredData$BaseL


#------------------------------------------------------------------------------------------------------------#
# Including data that has both historical information and projections, but does not depend on user's choices #
#------------------------------------------------------------------------------------------------------------#
# GDR relative to base (equal for all scenarios)

# Function 2: PrepareGDPRelativeToBase (T04-1)
# DB            <- PrepareGDPRelativeToBase function
# DD = FullHistoricalDataset
RawGDPRelativeToBase = RInputs_RawGDPRelativeToBase

write_csv(RInputs_RawGDPRelativeToBase, "data_py/RawGDPRelativeToBase.csv")
write_csv(RInputs_InternationalPrices_RegionMarket, "data_py/InternationalPrices_RegionMarket.csv")
write_csv(RInputs_InternationalPrices_IntPrices, "data_py/InternationalPrices_IntPrices.csv")
write_csv(RInputs_InternationalPrices_RegionAssumptions, "data_py/InternationalPrices_RegionAssumptions.csv")

# Base year for monetary values: To be read as a user-defined parameter
# Projections of GDP growth can be recomputed based on this
BaseY               <- BaseList$MonBaseYear
LU                  <- BaseList$LU

# Loading raw data that has 2018 as base year
# This information does not cover the entire time span needed
# RawGDPRelToBase     <- read_excel("data/Macro/GDPRelativeToBase.xlsx") %>%
#   filter(Year %in% all_of(BaseL$AllYears))

# NOTE: Dropping the dependency on Excel files
RawGDPRelToBase     <- RawGDPRelativeToBase %>%
  dplyr::filter(Year %in% all_of(BaseList$AllYears))

# Finding the GDPFactor for the chosen base year
GDPFactBaseYear     <- RawGDPRelToBase %>%
  dplyr::filter(Year == BaseY) %>%
  rename('BYGDPF' = GDPFactor) %>%
  select(CountryCode, BYGDPF)

# Re-scaling the GDP factor so that the base year = 1
GDPRelToBase        <- RawGDPRelToBase %>%
  left_join(GDPFactBaseYear, by = 'CountryCode') %>%
  mutate(GDPFactor = GDPFactor/BYGDPF,
         Year = as.character(Year)) %>%
  select(-BYGDPF) %>%
  pivot_wider(names_from = 'Year', values_from = 'GDPFactor')


# 2 steps for the adjustment:
# - Computing the last available GDP growth rate
# - Building a set of factors assuming that rate remains constant for the remaining years

# This assumes there is info for all countries for that year, which is currently the case (for all countries in this dataset)
LastYearAvailable   <- as.character(max(RawGDPRelToBase$Year))

Step1.1             <- GDPRelToBase %>%
  select(CountryCode,
         as.character(all_of(as.numeric(LastYearAvailable))-1),
         all_of(LastYearAvailable)) %>%
  set_names(c('CountryCode', 'Previous', 'Last')) %>%
  # Gross GDP growth rate. Assumed constant in the remaining years
  mutate(LastGDPgrowth = Last/Previous) %>%
  select(CountryCode, LastGDPgrowth)

# Expanding the rates found for all countries, sectors and fuels
Step1.2             <- BaseList$IDcols %>%
  left_join(Step1.1, by = 'CountryCode')


# Computing the new GDP factors for all years after the
# Filling a template matrix with information up to the last available year
Step2               <- BaseList$TemplMat
ColsToFill          <- which(colnames(Step2) %in% unique(RawGDPRelToBase$Year))

Step2[,ColsToFill]  <- as.matrix(BaseList$IDcols %>%
                                   left_join(GDPRelToBase, by = 'CountryCode') %>%
                                   select(-c(CountryCode, SectorCode, FuelCode)))

for(tt in c((as.numeric(LastYearAvailable)+1):BaseList$LastModYear)){ #tt<- 2034
  pos           <- which(colnames(Step2) == tt)
  Step2[, pos]  <- Step2[, pos-1]*Step1.2$LastGDPgrowth
}

# Building the data in matrix column format
ExpGDPFactor        <- tibble(BaseList$IDcols,
                              'GDPFactor' = Step2 )

# GH: output of PrepareGDPRelativeToBase
# Including the new data into the D matrix:
DB                  <- DD %>%
  left_join(ExpGDPFactor, by = c('CountryCode', 'SectorCode', 'FuelCode'))


# List of datasets: One dataset for each scenario.
# They can be later on combined with and rbind, by including an extra column to signal the scenario
DL              <- list()

ScenarioNames   <- names(MTI)

#-------------------------------------------------------------------------------#
# Keeping track of historic data available beyond the chosen first modeled year #
#-------------------------------------------------------------------------------#

LastYearDomPrices     <- as.numeric(last(names(which( colSums(DB$p, na.rm = TRUE) > 0 ))))

ss <- ScenarioNames[1]

# Initializing the data for the scenario
DL[[ss]]    <- DB

#------------------------------------------------------------------------------------------#
# Variables with historical data and projections, where the latter depend on user's inputs #
#------------------------------------------------------------------------------------------#

# Data on international prices, after the user has selected a data source

# PreprocessIntPricesList(
#   BaseList, RInputs_InternationalPrices_RegionAssumptions,
#   RInputs_InternationalPrices_RegionMarket, RInputs_InternationalPrices_IntPrices)

# Loading the lookups:
LU        <- BaseList$LU

# Several steps needed:
# Step1: Historical data and projection of prices for oop, coa and nga
#       - Expanding information by country, sector, fuel, and for all years
# Step2: Historical data for gso, die, lpg and ker, and expansion by country, sector and fuel
#       - After data on regional prices for gso, die, lpg and ker is read, it is expanded by country instead of region
#       - This is done for the full expansion Country, Sector, Fuel. Filter afterwards to keep only relevant fuels
#       - Expanding data for all required years
# Step3: Set the AdValFixed vector, at expanded level by country, sector and fuel, as a separate tibble
# Step4: Report data as a list


#------------#
#-- STEP 1 --#
#------------#

# Information on which region applies for each country, and how to consider taxes (fixed or ad valorem)
# IntPr_RegMarket   <- read_excel('data/Prices/RInputs_InternationalPrices.xlsx', sheet = 'RegionMarket') %>%
#                       select(-Country) %>%
#                       # Adopting PascalCase convention
#                       rename('AdValFixed' = `Baseline taxes are ad-valorem or fixed?`,
#                              'Market' = MarketAssumption)

IntPr_RegMarket   <- RInputs_InternationalPrices_RegionMarket %>%
  select(-Country) %>%
  # Adopting PascalCase convention
  rename('AdValFixed' = `Baseline taxes are ad-valorem or fixed?`,
         'Market' = MarketAssumption)

# Matching the first set of data (multiple sources) by region
# This step still provides information in nominal terms
RawStep1            <- RInputs_InternationalPrices_IntPrices %>%
  # Dropping some metadata columns:
  select(-c(SourceDescription, FuelDescription, Unit)) %>%
  mutate(across(-c('Source':'Market'), as.numeric)) %>%
  # To avoid receiving the message: "NAs introduced by coercion"
  suppressWarnings() %>%
  pivot_longer(-c('Source':'Market'), names_to = 'Year', values_to = 'Value') %>%
  # Expanding to FuelCode is more robust than renaming the column
  left_join(LU$FuelTypes, by = 'FuelType') %>%
  select(Source, FuelCode, Market, Year, Value) %>%
  dplyr::filter(Year %in% BaseList$AllYears) %>%
  pivot_wider(names_from = 'Year', values_from = 'Value') %>%
  rename('MarketRaw' = Market) %>%
  # Adding information on mappings to country, regions and markets
  # GH: Check if it is indeed many-to-many
  left_join(LU$ExpMarketIntPr, by = 'MarketRaw', relationship = "many-to-many") %>%
  left_join(IntPr_RegMarket, by = 'Market', relationship = "many-to-many") %>%
  right_join(BaseList$IDcols, by = c('CountryCode', 'FuelCode'), relationship = "many-to-many") %>%
  # AdValFixed could be recorded to a separate file if needed
  select(Source, CountryCode, SectorCode, FuelCode, where(is.numeric))


# Fully expanding the data to country, sector and fuel for oop, coa and nga
# Still in nominal terms
TempStep1.1         <- BaseList$IDcols %>%
  left_join(RawStep1, by = c('CountryCode', 'SectorCode', 'FuelCode') ) %>%
  dplyr::filter(FuelCode %in% c('oop', 'coa', 'nga'))


# TempStep1.2: Historical data for oop, coa and nga converted into real terms
# NOTE: Only THESE international prices are transformed into real terms using the GDP deflator instead of the CPI
TempStep1.2         <- TempStep1.1 %>%
  pivot_longer(cols = -c(CountryCode, SectorCode, FuelCode, Source),
               names_to = 'Year', values_to = "Value") %>%
  left_join(BaseList$DiscountFactorDefl, by = "Year") %>%
  rename("CurrentValue" = Value) %>%
  mutate(Value = CurrentValue * DiscountFactor) %>%
  select(-c(CurrentValue, DiscountFactor)) %>%
  pivot_wider(names_from = 'Year', values_from = 'Value')

# International prices of oil, coal and natural gas:
# Completing the information for missing years.
# We have data for the entire period, so the process should be redundant
Step1               <- CompleteYears(TheDF = TempStep1.2,
                                     LocalBaseL = BaseList,
                                     ProjY = 'ConstantValue')

#------------#
#-- STEP 2 --#
#------------#

# Prices of fuels to be assigned by region (This applies to: gso, die, lpg and ker)
# IntPr_RegAssum  <- read_excel('data/Prices/RInputs_InternationalPrices.xlsx', sheet = 'RegionAssumptions') %>%
#                     select(-FuelName) %>%
#                     pivot_longer(-c('Region':'FuelType'), names_to = 'Year', values_to = 'Value') %>%
#                     # Expanding the data from FuelType to FuelCode (instead of forcing a change of column name)
#                     # This shouldn't change the dimensions of the current table, as it has no 'ele'
#                     left_join(LU$FuelTypes, by = 'FuelType') %>%
#                     select(Region, FuelCode, Year, Value) %>%
#                     pivot_wider(names_from = 'Year', values_from = 'Value')

# NOTE: Dropping the dependency on Excel files
# load('data/RInputs_InternationalPrices_RegionAssumptions.rda')
IntPr_RegAssum  <- RInputs_InternationalPrices_RegionAssumptions %>%
  select(-FuelName) %>%
  pivot_longer(-c('Region':'FuelType'), names_to = 'Year', values_to = 'Value') %>%
  # Expanding the data from FuelType to FuelCode (instead of forcing a change of column name)
  # This shouldn't change the dimensions of the current table, as it has no 'ele'
  left_join(LU$FuelTypes, by = 'FuelType') %>%
  select(Region, FuelCode, Year, Value) %>%
  pivot_wider(names_from = 'Year', values_from = 'Value')


# TempStep2: Historical data for gso, die, lpg and ker
# In nominal terms
TempStep2       <- BaseList$IDcols %>%
  left_join(LU$CountryCode, by = 'CountryCode') %>%
  select(CountryCode, SectorCode, FuelCode, Region) %>%
  left_join(IntPr_RegAssum, by = c('Region', 'FuelCode')) %>%
  dplyr::filter(FuelCode %in% c('gso', 'die', 'lpg', 'ker')) %>%
  select(-Region)

# Step2: Historical data for gso, die, lpg and ker converted into real terms
# NOTE:   The completion of this data set depends on the price source selected by the user
#         This, as the evolution of these prices depends on the evolution of oil prices
Step2           <- TempStep2 %>%
  pivot_longer(cols = -c(CountryCode, SectorCode, FuelCode),
               names_to = 'Year', values_to = "Value") %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(BaseList$DiscountFactorCPI, by = "Year") %>%
  rename("CurrentValue" = Value) %>%
  mutate(Value = CurrentValue * DiscountFactor) %>%
  select(-c(CurrentValue, DiscountFactor)) %>%
  pivot_wider(names_from = 'Year', values_from = 'Value')


#------------#
#-- STEP 3 --#
#------------#

# AdValFixed tibble by Country, Sector and Fuel
# NOTE: This extends the market category for all fuels, despite not having an international price for some of them
Step3               <- BaseList$IDcols %>%
  left_join(IntPr_RegMarket, by = 'CountryCode') %>%
  select(CountryCode, SectorCode, FuelCode, AdValFixed)


#------------#
#-- STEP 4 --#
#------------#

# Reporting all variables as a list for further consolidation after receiving the user's choices
# Prices from single and multiple sources have all been converted into real terms

IntPrices                   <- list()
IntPrices$IPMultSources     <- Step1            # information for oop, coa and nga (multiple sources)
IntPrices$IPSingleSource    <- Step2            # information for gso, die, lpg and ker (single source)
IntPrices$TibAdValFixed     <- Step3            # Tibble with the AdValFixed vector

# return(IntPrices)

IPList <- IntPrices

# IPList contains international prices from single/multiple sources, in real terms
# It is computed on the main function, after receiving the input from the user on the selected source

DL[[ss]]    <- PrepareInternationalPrices(
  DD        = DL[[ss]], BaseL     = BaseList,
  IPList    = PreprocessIntPricesList(
    BaseList, RInputs_InternationalPrices_RegionAssumptions,
    RInputs_InternationalPrices_RegionMarket, RInputs_InternationalPrices_IntPrices),
  SelSource = MTI[[ss]]$IntPricesSource)

SelSource = MTI[[ss]]$IntPricesSource

#------------#
#-- STEP 1 --#
#------------#

# This fuels are done first, as they depend on the user's choice of source, and they affect the evolution of other fuels
# Treating the international price data that for coa, nga and oop
# Reading and creating a "layer" of international prices for the data section that will be affected by the user's choice


# If the user provides a non-useful input, we resort to 'IMF-IEA' by default
if(!SelSource %in% unique(IPList$IPMultSources$Source)){
  SelSource   <- 'IMF-WB'
}

# Filtering the information according to the source selected by the user
FilStep1        <- IPList$IPMultSources %>%
  dplyr::filter(Source == SelSource) %>%
  select(-Source)

# Expanding this to all possible combinations of country, fuel and sector, so that NA will appear.
Step1           <- BaseList$IDcols %>%
  left_join(FilStep1, by = c('CountryCode', 'SectorCode', 'FuelCode'))


#------------#
#-- STEP 2 --#
#------------#

# Treating the international price data that for die, gso, lpg and ker
# Reading and creating a "layer" of international prices for the data section that is not affected by the user's choice

# These prices come from different markets/regions, but are mapped already to each country, sector and fuel
# However, projections depend on oil prices, which come from multiple sources

# Vector with selected oil prices (in real USD per bbl)
OilPrices     <- Step1 %>%
  dplyr::filter(FuelCode == 'oop') %>%
  select(-c(CountryCode, SectorCode, FuelCode)) %>%
  distinct()

# Completing the time series for all years:
TempStep2     <- CompleteYears(TheDF = IPList$IPSingleSource,
                               LocalBaseL = BaseL,
                               ProjY = 'Relative',
                               ReferenceVal = OilPrices)

# Expanding this to all possible country, fuel and sector, so that NA will appear.
Step2         <- BaseList$IDcols %>%
  left_join(TempStep2, by = c('CountryCode', 'SectorCode', 'FuelCode'))


#------------#
#-- STEP 3 --#
#------------#

# Joining both layers from Steps 1 and 2
# coalesce takes the inputs from the first element, unless they are NAs, in which case it takes it from the second element
# International prices of non-fossil fuels will appear as NA
TempStep3.1     <- coalesce(Step1, Step2)

# These prices are all expressed in multiple units. Some of those need to be converted into USD/GJ
# Conversion factors aftect the energy unit, which is on the denominator of the units, hence the division
TempStep3.2     <- TempStep3.1 %>%
  pivot_longer(cols = -c(CountryCode, SectorCode, FuelCode), names_to = 'Year', values_to = 'Value') %>%
  pivot_wider(names_from = 'FuelCode', values_from = 'Value') %>%
  mutate(coa = coa / as.numeric(BaseList$LU$ConvFact %>%
                                  dplyr::filter(FuelCode == 'coa') %>%
                                  select(ConversionFactor) ),
         nga = nga / as.numeric(BaseList$LU$ConvFact %>%
                                  dplyr::filter(FuelCode == 'nga') %>%
                                  select(ConversionFactor) ),
         oop = oop / as.numeric(BaseList$LU$ConvFact %>%
                                  dplyr::filter(FuelCode == 'oop') %>%
                                  select(ConversionFactor) )) %>%
  pivot_longer(cols = -c(CountryCode, SectorCode, Year), names_to = 'FuelCode', values_to = 'Value') %>%
  pivot_wider(names_from = 'Year', values_from = 'Value')

# Transforming this into D matrix format
# At this stage, prices are already in real terms per GJ (or liter, for the respective fuels)
# TempStep3 was not ordered by IDcols, so Step 3 has to be built as this:
Step3           <- tibble((TempStep3.2 %>% select(CountryCode, SectorCode, FuelCode)),
                          'IntPrices'  = as.matrix(TempStep3.2 %>% select(-c('CountryCode', 'SectorCode', 'FuelCode'))))

# Including this into the D matrix
DD              <- DD %>%
  left_join(Step3, by = c('CountryCode', 'SectorCode', 'FuelCode'))

#------------#
#-- STEP 4 --#
#------------#

# Including the AdValFixed data into the D matrix
DD              <- DD %>%
  left_join(IPList$TibAdValFixed, by = c('CountryCode', 'SectorCode', 'FuelCode'))


# Among the helpers used for domestic price forecasting, one requires international prices data:
t0              <- which(colnames(DD$cs) == BaseList$FirstModYear) - 1

# The inputs to compute the helper often include multiple NA
# The process applied here is more robust than simpler ones tested
TempAltFalseH4  <- data.frame('FalseH4' =  DD$cs[,t0] - (DD$PCtrl_alpha0 -1)*(DD$IntPrices[,t0] - DD$IntPrices[,t0-1]) ) %>%
  mutate('Zero' = 0,
         # Setting NAs in tempH4 to 0:
         'tempH4noNA' = coalesce(FalseH4, Zero)) %>%
  select(-FalseH4)

# Computing the minimum between the formula and 0
# Recall that cs is the negative of the consumer subsidy in the IMF dataset
AltFalseH4      <- apply( TempAltFalseH4,
                          1,
                          FUN = min,
                          na.rm = TRUE)

DD$H4           <- if_else(DD$cs[,t0] == 0,
                           # If true:
                           0,
                           # If false:
                           AltFalseH4)

#---------------------------------------------------------------------------------------#
# Dummy data for additional variables/parameters required:                              #
#---------------------------------------------------------------------------------------#

# -----
# Post-Pol GDP growth (scenario-dependent)
# Proper calculations require the calculations of revenues, revenue recycling and the computing if their effect through multipliers
tempDummy                       <-DL[[ss]]$GDPFactor[,-1] / DL[[ss]]$GDPFactor[,-dim(DL[[ss]]$GDPFactor)[2]] - 1

# Including an extra column for the first year, and replacing the first column ('Inf') by '0':
tempDummy[,1]                   <- 0
tempDummy                       <- cbind(0, tempDummy)
colnames(tempDummy)             <- colnames(DL[[ss]]$GDPFactor)
DL[[ss]]$PostPolGDPgrowth       <- tempDummy


# -----
# Discount factor (proportion of estimated to average calibrated income elasticity)
# Used in the mitigation equation. Relies on GDP post policy, so fixed to 1 for now
DL[[ss]]$DiscFactorMitEQ        <- 1

#---------------------------------------------------------------------------------------#
# Reading the policy inputs                                                             #
#---------------------------------------------------------------------------------------#

# If ss == ScenarioNames[1]

# New carbon tax under the baseline
DL[[ss]]$NCT        <- BaseList$TemplMat
# Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
DL[[ss]]$NCTcov_sf  <- BaseList$TemplMat

# New ETS under the baseline
DL[[ss]]$NETSP      <- BaseList$TemplMat
# Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
DL[[ss]]$NETScov_sf <- BaseList$TemplMat

# If ss == ScenarioNames[2]
# New Carbon tax under the policy (assumed equal for all countries)
DL[[ss]]$NCT        <- CreateCP(StartCPYear  = MTI[[ss]]$CTintroYear,
                                TargetCPYear = MTI[[ss]]$CTtargetYear,
                                StartCP      = MTI[[ss]]$CTintroValue,
                                TargetCP     = MTI[[ss]]$CTtargetValue,
                                TemplMat     = BaseList$TemplMat)

# Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
DL[[ss]]$NCTcov_sf  <- MTI[[ss]]$NCTcov_sf

# New ETS permit under the policy
DL[[ss]]$NETSP      <- CreateCP(StartCPYear  = MTI[[ss]]$ETSintroYear,
                                TargetCPYear = MTI[[ss]]$ETStargetYear,
                                StartCP      = MTI[[ss]]$ETSintroValue,
                                TargetCP     = MTI[[ss]]$ETStargetValue,
                                TemplMat     = BaseList$TemplMat)

# Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
DL[[ss]]$NETScov_sf <- MTI[[ss]]$NETScov_sf

#---------------------------------------------------------------------------------------#
# Mitigation equation and Price algorithm: iterations by time step                      #
#---------------------------------------------------------------------------------------#

for(yy in c(BaseList$FirstModYear:BaseList$LastModYear)){  # yy <- BaseList$FirstModYear


  if( yy > LastYearDomPrices){

    # To avoid overwriting the existing observed information
    # Applying the price algorithm to the current data
    DL[[ss]]          <- ForecastDomPrices(DD                  = DL[[ss]],           # D matrix for the current scenario
                                           BaseL               = BaseList,
                                           PolInputs           = MTI[[ss]],          # List of all elements detailing the policy applied
                                           ScenName            = ss,
                                           Year                = yy,
                                           LastHistYear        = LastYearDomPrices)
  }

  # Applying the mitigation equation on top of the newly computed prices
  DL[[ss]]          <- MitEQ(DD                    = DL[[ss]],           # D matrix for current scenario
                             BaseL                 = BaseList,
                             PolInputs             = MTI[[ss]],          # List of all elements detailing the policy applied
                             Year                  = yy)

  # Computing Emissions
  # Note:
  #   - The function has been written to apply it year by year, but it could be applied to the full matrix afterwards
  DL[[ss]]          <- Emissions(DD                = DL[[ss]],           # D matrix for current scenario
                                 BaseL             = BaseList,           # Unused here
                                 PolInputs         = MTI[[ss]],          # Unused here
                                 Year              = yy,
                                 AdjFactor         = 1.12)
}

# Scenario1, no option to get UserScenario
Sc1 <- TestSingleCountry$Scenario1

# Running CPAT for two countries, for two scenarios:
TestMultiCountry    <- cpatr::SimpleCPAT(FullHistoricDataset = DB_AllCountries,
                                         FullBaseList        = BaseList_AllCountries,
                                         UserScen            = UserScenario,
                                         CountryList         = c('USA', 'ZAF'))

# After the conversion, notice it is convenient to rename the numerical column to take the name of the scenario
BaselineResults     <- Convert_D_to_T(DD = TestMultiCountry$Scenario1) %>%
  rename('Baseline' = 'Value')

# Policy scenario, including the renaming of the numerical column
PolicyResults       <- Convert_D_to_T(DD = TestMultiCountry$Scenario2) %>%
  rename('CarbonTax' = 'Value')

