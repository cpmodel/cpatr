
#-------------------------------------------------------------------------------
# Name: MTOutputAnalysis.R
# Purpose: Use data for analysis
# Change Log
# Version Initials Date Change
# 0.1 FG 20-May-22 Creation
# 0.2 SS 20-May-22 Modification and finalisation

#-------------------------------------------------------------------------------
 rm(list=ls())

library(dplyr)
library(tidyverse)
library(magrittr)
library(readr)

#EXAMPLE 1: SIMPLE EXTRACT OF DATA###############################################################################
IndicatorNeeded='elec'
LongFileName <- paste0('Latest_Long_',IndicatorNeeded)
#Read .rmd file already transformed into long format which you want to use as your input.
MTSelectedOutputLong <- read_rds(paste0('data/MTOutputData/2_Processed/',LongFileName,'.rds'))
View(MTSelectedOutputLong)

TSFileName <- paste0('Latest_TimeSeries_',IndicatorNeeded)
#Read .rmd file already transformed into long format which you want to use as your input.
MTSelectedOutputTS <- read_excel(paste0('data/MTOutputData/2_Processed/',TSFileName,'.xlsx'))
View(MTSelectedOutputTS)


#EXAMPLE 2: LOAD WHOLE DATASET AND FILTER################################################################################

# Change here to match the name of the file to be used as input.
MTFileName <- 'Latest_Long_Selected_Indicators'

# Vector containing the sequence of years included in MTOutput
AllYears <- as.character(2019:2036)
PurposeDef <- 'Analysis'
#################################################################################
#Read .rmd file already transformed into long format which you want to use as your input.
MTSelectedOutput <- read_rds(paste0('data/MTOutputData/2_Processed/Latest_Long_All_Indicators.rds'))

# Example: Total energy consumption by fuel, for both subscenarios
#(you can select any variable by filtering it by variable code used in the MTOutputs (ener, elec, emis)
#you can select SubScenNo ==1=Baseline ==2=Policy, or delete that part to produce results for both)
TotEnerCons <- MTSelectedOutput %>%
  filter(Variable == 'ener') %>%
  distinct_all() %>%
  filter(grepl("Total consumption by fuel type", CPATIndicator),
         SubScenNo == 1)
# Verify the Indicators included are the desired ones:
unique(TotEnerCons$CPATIndicator)
# Extracting relevant info and Transforming it to wide format (time series):
TotEnerConsSelected <- TotEnerCons %>%
  select(Scenario,Purpose,Variable,CPATIndicator,RunDate,CountryCode,Sector,FuelType,Year,SubScenario,Value) %>%
  pivot_wider(names_from = Year, values_from = Value)# Checking the New table

head(TotEnerConsSelected)

# Save you file in your prefered location in RDS or .xlsx format

