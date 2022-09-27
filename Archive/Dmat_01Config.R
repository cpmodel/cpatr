#############################
# LOADING REQUIRED PACKAGES #
#############################

library(tidyverse)
library(magrittr)
library(readxl)
library(dplyr)


################################
# LOOKUPS AND GLOBAL VARIABLES #
################################

# Conversion factor required to transform international prices to CPAT format
PJPerktoe               <- 0.041868
# TBC: create a file (Add tab to Lookup?) with all relevant conversion factors


# Input all relevant lookups from centralized file:
Lookups                 <- list()

Lookups$CountryCode     <- read_excel("data/Lookups/Lookups.xlsx", sheet = "CountryCode")
Lookups$EFCompleting    <- read_excel("data/Lookups/Lookups.xlsx", sheet = "EmissionFactorsCompleting")
Lookups$LargeFuelList   <- read_excel("data/Lookups/Lookups.xlsx", sheet = "FuelCodes")
Lookups$FuelTypes       <- read_excel("data/Lookups/Lookups.xlsx", sheet = "FuelTypes")
Lookups$PowFuelTypes    <- read_excel("data/Lookups/Lookups.xlsx", sheet = "PowerFuelTypes")
Lookups$ProjFuelTypes   <- read_excel("data/Lookups/Lookups.xlsx", sheet = "ProjectionsFuelTypes")
Lookups$SectorGroups    <- read_excel("data/Lookups/Lookups.xlsx", sheet = "FlowSectorGroupMapEC")
Lookups$RegionETS       <- read_excel('data/Lookups/Lookups.xlsx', sheet = "Region_ETScountries")
Lookups$ExpMarketIntPr  <- read_excel('data/Lookups/Lookups.xlsx', sheet = "IntPricesMarkets")
Lookups$SectorElast     <- read_excel('data/Lookups/Lookups.xlsx', sheet = "SectorElasticities")
Lookups$FuelElast       <- read_excel('data/Lookups/Lookups.xlsx', sheet = "FuelElasticities")


# Replacing previously hard-coded vectors by extractions from lookups

# NOTE: 'ore' appears twice. # Do not use bgs, bdi, obf, heat for MEq
FuelList                <- unique(Lookups$FuelTypes$FuelCode)

# NOTE: 'ore' appears twice as a FuelCode (under "ore" and under "ecy")
PrimaryFuelList         <- unique(Lookups$FuelTypes$FuelCode)

SectorCodeList          <- unique(Lookups$SectorGroups$SectorCode)

# Pre-selecting a list of countries covered in the mitigation module (data coverage)
CoveredCountryList      <- Lookups$CountryCode %>%
                            filter(CountryInMitigationMod == 'T') %>%
                            select(CountryCode) %>%
                            distinct()


######################################
##  CUSTOM FUNCTIONS FOR OPERATIONS ##
######################################

# Defining custom functions: Element-by-element operations that exclude NA from the calculations

# Addition:
`%+%`         <- function(x, y)  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))

# Product
`%x%`         <- function(x, y)  mapply(prod, x, y, MoreArgs = list(na.rm = TRUE))

# Division
`%/%`         <- function(x, y)  mapply(prod, x, 1/y, MoreArgs = list(na.rm = TRUE))


# Calling the rest of scripts with all relevant functions
source('integration/Dmat_02UserInputs.R')
source('integration/Dmat_03PrepareHistData.R')


# This would need to be called afterwards, once the base dataset is created, as this one depends on user's inputs
source('integration/Dmat_04DataInclExogProj.R')
