##-----------------------------------------------------------------------------
# Name:     2_MitigationConfig
# Purpose:  Script loading relevant packages and pre-processing functions

# Change Log
# Version   Initials    Date        Change
# 0.1       DB          31-Jan-22   Creation of file
##-----------------------------------------------------------------------------

#############################
# LOADING REQUIRED PACKAGES #
#############################

# rm(list=ls())

library(tidyverse)
library(magrittr)
library(readxl)


################################
# LOOKUPS AND GLOBAL VARIABLES #
################################

# Global starting year for data should be 2000, and the base year 2019. Data should run until 2050
BaseYear            <- 2018
StartYear           <- 1990
EndYear             <- 2019
PJPerktoe           <- 0.041868


# Input all relevant lookups from centralized file:
Lookups                 <- list()

Lookups$CountryCode     <- read_excel("../data/Lookups/Lookups.xlsx", sheet = "CountryCode")
Lookups$EFCompleting    <- read_excel("../data/Lookups/Lookups.xlsx", sheet = "EmissionFactorsCompleting")
Lookups$LargeFuelList   <- read_excel("../data/Lookups/Lookups.xlsx", sheet = "FuelCodes")
Lookups$FuelTypes       <- read_excel("../data/Lookups/Lookups.xlsx", sheet = "FuelTypes")
Lookups$PowFuelTypes    <- read_excel("../data/Lookups/Lookups.xlsx", sheet = "PowerFuelTypes")
Lookups$ProjFuelTypes   <- read_excel("../data/Lookups/Lookups.xlsx", sheet = "ProjectionsFuelTypes")
Lookups$SectorGroups    <- read_excel("../data/Lookups/Lookups.xlsx", sheet = "FlowSectorGroupMapEC")
# TBC: Include conversion factors


# Replacing previously hard-coded vectors by extractions from lookups

# TBC: 'ore' appears twice. # Do not use bgs, bdi, obf, heat for MEq
FuelList                <- unique(Lookups$FuelTypes$FuelCode)

# TBC: Note, 'ore' appears twice as a FuelCode (under "ore" and under "ecy")
PrimaryFuelList         <- unique(Lookups$FuelTypes$FuelCode)

# TBC: Do we keep "wav" and "neu"?
SectorCodeList          <- unique(Lookups$SectorGroups$SectorCode)

# TBC: Full country list that also includes WORLDAV and WORLDMAR.
# TBC: USe this or a constrained list of countries?
FullCountryList         <- unique(Lookups$CountryCode$CountryCode[Lookups$CountryCode$Type == 'Country'])


##################################
# LOADING PRE-PROCESSING SCRIPTS #
##################################



