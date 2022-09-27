#-------------------------------------------------------------------------------
# Name:     PR1_DomesticPricesPreProcessing
# Purpose:  Transform and expand raw fossil fuel prices into CPAT R format

# Change Log
# Version   Initials    Date        Change
# 0.1       DB          18-Apr-22   Creation
#-------------------------------------------------------------------------------

# rm(list=ls())

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading required packages
library(dplyr)
library(tidyverse)
library(magrittr)
library(readxl)


# Name of latest/required file with domestic prices (without xlsx extension explicitely mentioned)
FilePrices        <- 'prices_for_cpat_all_years_2022_04_15'


############################
# INTERMEDIATE OUTPUT FILE #
############################

# This section creates a transformed version of the raw table that includes:
# - Changing data to a long format
# - Adjusting all numeric variables to 'double'
# - Changing names to match CPAT conventions

# NOTE: At this stage, vatrate variables are kept and sector 'all' has not been expanded
head(RawHistDomPrices)
# Transforming the table to long format, with changed names and separated CPAT code
RawHistDomPrices  <- read_excel(paste0('data/Prices/1_Raw/',FilePrices,'.xlsx')) %>%
                      select(-c(country_year,
                                countrycode_weo,
                                starts_with('floating_'))) %>%
                      rename(CountryCode = countrycode,
                             Country = countryname,
                             Year = year) %>%
                      mutate(across(.cols= starts_with("mit_"), .fns = as.numeric)) %>%
                      select(CountryCode, Country, Year, starts_with('mit_')) %>%
                      pivot_longer(cols = starts_with('mit_'),  names_to = 'OldCPATCode', values_to = 'Value') %>%
                      separate(OldCPATCode, into = c('Module', 'Indicator', 'FuelType', 'SectorGroup')) %>%
                      select(-Module)


# Storing this intermediate output for users needing the original table in long format
saveRDS(RawHistDomPrices, paste0('2_Processed/Long_',FilePrices))


###################
# EXPANDED PRICES #
###################

# This section expands sector categories to match CPAT's level of detail
# vatrate is no longer kept

# Reading lookups and required additional files
Lookups                 <- list()

Lookups$SectorGroups    <- read_excel("../../data/Lookups/Lookups.xlsx", sheet = "FlowSectorGroupMapEC")

# Adapting current data into format needed for expansion
ExpandHistDomPr   <- RawHistDomPrices %>%
                      filter(Indicator != 'vatrate') %>%
                      left_join(Lookups$SectorGroups, by = 'SectorGroup') %>%
                      select(-c(SectorGroup, FlowCode))

# Storing this final output for users needing the expanded price information
saveRDS(ExpandHistDomPr, paste0('2_Processed/Expand_',FilePrices))
