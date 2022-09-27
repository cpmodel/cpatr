#-------------------------------------------------------------------------------
# Name: 1_Mitigation
# Purpose: Script developing the main calculations in the Mitigation Module
# Note: All functions are developed in '3_MitigationFunctions.R'


# Change Log
# Version  Initials  Date  Change
# 0.1 DB 31-Jan-22  Creation of file
# 0.2 DB 02-Feb-22  Adapting Stephen's developments to the file structure
#
#-------------------------------------------------------------------------------

############################
# USE CASES #
############################
# 1. Direct use of the R package (product A) (Main use case for now)
# 2. API Call (from Excel) -focus here (single country use)
# 3. Shiny use or internal of this function (whole world), using R alone without any reference to CPAT-Excel

# Model options:
# 1. Update the carbon tax in D with the new carbon tax that is defined from the Table of model configuration
# 2. (Update the elasticities which could be from multiple sources) (Not required in first version)
# 3. (Update the emission factors which could be from multiple sources)
# 4. Create Prices
# 5. Iterate the energy consumption


rm(list=ls())

# Setting working directory to the location of this script
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(magrittr)
library(readxl)

RunProductA = function() {

BaseYear <- 2019
StartYear <- 2019
EndYear <- 2035
PJPerktoe <- 0.041868

# Loading required configuration, packages and processed data
source('R/ME2_UserLevelFunctions.R')

#######################################
# LOADING REQUIRED PRE-PROCESSED DATA #
#######################################
# LoadDatasetFromFile <- function(fileName='../data/ME_ProcessedDataD.rds') {
#  read_rds(fileName)
# }
# D <- LoadDatasetFromFile()

path_processed_data <- 'data/ME_ProcessedDataD.rds'
D <- read_rds(path_processed_data)

#D <- ObtainDataset()
# Run the following alternative if aiming at re pre-processing the data


#####################
# CARBON TAX SERIES #
#####################

CT <- CreateCT(StartCTYear = 2022,
               TargetCTYear = 2030,
               StartCT = 60,
               TargetCT = 100,
               InitYear = StartYear,
               FinYear = EndYear)

# Adding CT to the Dataset
D <- D %>%
  left_join(CT, by = character()) #Joining by a blank field creates a cartesian product n rows m ->n*m
#Turn into function

######################
# PRICES AFTER TAXES #
######################


D <- AddCarbonTaxToPrices(D)


################################
# RESULTING ENERGY CONSUMPTION #
################################

D <- MitEQ(TempD = D,
           InitY = StartYear,
           EndY = EndYear)


output(D)
}

#Function to code: Revenue function - ask Alexandra to define this
#Function to code: Emissions function


#Function: Output as time series in 'CPAT TS form' (Long in variables, wide in time)
#Convert Back To Long (for GG Plot) (Fully Long in both variables, time, and dimensions)
#AggregationConvenience Functions (Better using the long form - eg aggregate to sector groupings)

