#-------------------------------------------------------------------------------
# Name:     1_Mitigation
# Purpose:  Script developing the main calculations in the Mitigation Module
# Note:     All functions are developed in '3_MitigationFunctions.R'


# Change Log
# Version   Initials    Date        Change
# 0.1       DB          31-Jan-22   Creation of file
# 0.2       DB          02-Feb-22   Adapting Stephen's developments to the file structure
#-------------------------------------------------------------------------------

rm(list=ls())

# Setting working directory to the location of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(magrittr)
library(readxl)

BaseYear            <- 2018
StartYear           <- 1990
EndYear             <- 2019
PJPerktoe           <- 0.041868

# Loading required configuration, packages and processed data
source('cal_ME2_UserLevelFunctions.R')

#######################################
# LOADING REQUIRED PRE-PROCESSED DATA #
#######################################
LoadDatasetFromFile <- function(fileName='../data/ME_ProcessedDataD2.rds') {
      read_rds(fileName)
}

#write_rds

D       <- LoadDatasetFromFile()



#D       <- ObtainDataset()
# Run the following alternative if aiming at re pre-processing the data


#####################
# CARBON TAX SERIES #
#####################

CT      <- CreateCT(StartCTYear  = 1990,
                    TargetCTYear = 2019,
                    StartCT      = 0,
                    TargetCT     = 0,
                    InitYear     = StartYear,
                    FinYear      = EndYear)

# Adding CT to the Dataset
D       <- D %>%
            left_join(CT, by = character()) #Joining by a blank field creates a cartesian product n rows m ->n*m

######################
# PRICES AFTER TAXES #
######################

D       <- D %>%
            rename('rp' = rp.obs) %>%
            mutate(atp = max(0,01, rp))   # Given no tax is applied in hindcasting. If not: ct*ef





################################
# RESULTING ENERGY CONSUMPTION #
################################

D       <- MitEQ(TempD = D,
                 InitY = StartYear,
                 EndY  = EndYear)



# Introducing checks to filter and keep only countries with full data:
DFiltered   <- D %>%
                mutate('checkEC'     = rowSums(ec),
                       'checkECobs'  = rowSums(ec.obs),
                       'checkNGDP_R' = rowSums(NGDP_R)) %>%
                filter(!is.na(checkEC),
                       !is.na(checkECobs),
                       !is.na(checkNGDP_R))


saveRDS(DFiltered, "DforCalibration")

############################
# USE CASES #
############################

#1. API Call (from Excel) -focus here (single country use)
#2. Shiny use or internal of this function (whole world), using R alone without any reference to CPAT-Excel

############################
# LOADING REQUIRED LOOKUPS #
############################

# 1. API CALL: Providing a) Function Name b) Input codes (country, sectors and fuel types) and c) Table of model configuration

# 2. Extract from the API call the model configuration and select the carbon tax created by that model configuration.

# 3. FILTER by JOINING D with the required input codes.

#Model options:
# 4. Update the carbon tax in D with the new carbon tax that is defined from the Table of model configuration
# 5. (Update the elasticities which could be from multiple sources) (Not required in first version)
# 6. (Update the emission factors which could be from multiple sources)
# 7. Create Prices
# 8. Iterate the energy consumption
#Input codes
#Column of model information

#Action:
# Select rows from D
# Output e.g ec from those rows in matrix form

