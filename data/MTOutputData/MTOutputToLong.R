#-------------------------------------------------------------------------------
# Name:     MT_MTOutputToLong.R
# Purpose:  Transform MTOutput to long/tidy version

# Change Log
# Version   Initials    Date        Change
# 0.1       DB          12-Apr-22   Creation

#-------------------------------------------------------------------------------


# rm(list=ls())

library(dplyr)
library(tidyverse)
library(magrittr)
library(readxl)


################################################################################
# Change here to match the name of the file to be used as input.
MTFileName      <- 'MTO_T5_WB5_Latest'

# Date in which the run was performed
DateToAdd       <- "16-05-2022"

# Vector containing the sequence of years included in MTOutput
AllYears        <- as.character(2019:2036)

PurposeDef      <- 'Calibration, forecasts'
################################################################################



# MT Output after deleting "Dist", renaming columns and transforming it into long format
RawMTOutput     <- read_excel(paste0('data/MTOutputData/1_Raw/',MTFileName,'.xlsx')) %>%
                    # Dropping excess columns:
                    select(-starts_with('Dist')) %>%
                    # Ensuring all data years are taken as numbers and not characters
                    mutate(across(.cols = all_of(AllYears), .fns = as.numeric )) %>%
                    distinct_all() %>%
                    separate(RCode, into = c('CountryCode', NA, NA, "Sector", "FuelType", "Other", "Unit", "SubScenNo")) %>%
                    # Transforming to longer version:
                    pivot_longer(cols = all_of(AllYears), names_to = 'Year', values_to = 'Value') %>%
                    # Adding the date in which this particular run was performed
                    mutate('RunDate' = DateToAdd,
                           'Purpose' = PurposeDef) %>%
                    select('Country':'Purpose')

# Example: Total energy consumption by fuel, for both subscenarios
TotEnerCons   <- RawMTOutput %>%
                  filter(Variable == 'ener') %>%
                  distinct_all() %>%
                  filter(grepl("Total consumption by fuel type", CPATIndicator),
                         SubScenNo == 1)

# Verify the Indicators included are the desired ones:
unique(TotEnerCons$CPATIndicator)

# Extracting relevant info and Transforming it to wode format (time series):
TotEnerCons     <- TotEnerCons %>%
                    select(CountryCode, FuelType, Year, Value) %>%
                    pivot_wider(names_from = Year, values_from = Value)

# Checking the New table
head(TotEnerCons)


# Adapting the name of the file, and storing it
MTname_short    <- gsub('Latest', DateToAdd, MTFileName)

# Saving the long format of the current data-set into the processed subfolder for future uses
saveRDS(RawMTOutput, paste0('data/MTOutputData/1_Processed/Long_',MTname_short))
