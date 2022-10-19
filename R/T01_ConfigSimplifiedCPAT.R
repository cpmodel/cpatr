# library(tidyverse)
# library(magrittr)
# library(readxl)
# library(dplyr)

#source('R/T01_ConfigSimplifiedCPAT.R)
#source('R/T02_CPATCoreFunctions.R')
#source('R/T03_SupportFunctions.R')
#source('R/T04_DataInclExogProj.R')

# In absence of user's inputs, I need a matrix of 0 with the proper dimensions. This is already included in BaseList
# load('data/BaseList.rda')

#-----------------------------------------------------------------------#
#- Temporarily introducing here some dummy replacement for user inputs -#
#-----------------------------------------------------------------------#

# NOTE
# - This relies on the the data structure created for the D matrix (BaseList)
# - Inputs from the user-interface should be loaded at this stage (script)
# - Given the way the files are built, we may need to run CPAT for all countries and only then filter for the selected ones


# ----------------------------- #
# Dummy inputs for the baseline #
# ----------------------------- #


########################################################
## BASELINE INPUT DEFAULTS FOR THE ALL-COUNTRIES CASE ##
########################################################

#' Baseline Inputs:
#' This function reads some pre-loaded scenarios to test CPAT
#'
#' @return
#' @export
#' @import tidyr dplyr purrr tidyverse readxl readr stringr
#' @param BaseL BaseList included in cpatr with templates

BaselineInputs              <- function(BaseL){


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
    Scenario1$NCTcov_sf         <- BaseL$TemplMat
    # Info on new ETS permit
    Scenario1$ETSintroYear      <- 2050
    Scenario1$ETSintroValue     <- 0
    Scenario1$ETStargetYear     <- 2050
    Scenario1$ETStargetValue    <- 0
    Scenario1$NETScov_sf        <- BaseL$TemplMat
    # Info on Existing policies
    Scenario1$ApplyExistingCP   <- TRUE
    Scenario1$PhaseOut_cs       <- 1-BaseL$TemplMat    # Phaseout consumer subsidy (% of subsidy kept)
    Scenario1$PhaseOut_ps       <- 1-BaseL$TemplMat    # Phaseout producer subsidy (% of subsidy kept)
    Scenario1$PhaseOut_pc       <- 1-BaseL$TemplMat    # Phaseout price controls (% of price control left)
    Scenario1$ShadowPrIncr      <- BaseL$TemplMat      # Shadow price annual growth rate
    Scenario1$CovShadowPrice    <- BaseL$TemplMat      # Percentage of shadow price
    Scenario1$ExogShockCOVID    <- BaseL$TemplMat      # Exogenous COVID shock

    # --------------------------- #
    # Dummy inputs for the Policy #
    # --------------------------- #

    Scenario2                   <- list()

    Scenario2$IntPricesSource   <- 'IMF-WB'
    Scenario2$AddExternalityVAT <- FALSE
    # Info on new Carbon Tax
    Scenario2$CTintroYear       <- 2023
    Scenario2$CTintroValue      <- 20
    Scenario2$CTtargetYear      <- 2030
    Scenario2$CTtargetValue     <- 100
    Scenario2$NCTcov_sf         <- 1-BaseL$TemplMat
    # Info on new ETS permit
    Scenario2$ETSintroYear      <- 2050
    Scenario2$ETSintroValue     <- 0
    Scenario2$ETStargetYear     <- 2050
    Scenario2$ETStargetValue    <- 0
    Scenario2$NETScov_sf        <- BaseL$TemplMat
    # Info on Existing policies and subsidies and price controls
    Scenario2$ApplyExistingCP   <- TRUE
    Scenario2$PhaseOut_cs       <- BaseL$TemplMat      # Phaseout consumer subsidy (% of subsidy kept)
    Scenario2$PhaseOut_ps       <- BaseL$TemplMat      # Phaseout producer subsidy (% of subsidy kept)
    Scenario2$PhaseOut_pc       <- BaseL$TemplMat      # Phaseout price controls (% of price control left)
    Scenario2$ShadowPrIncr      <- BaseL$TemplMat      # Shadow price annual growth rate
    Scenario2$CovShadowPrice    <- BaseL$TemplMat      # Percentage of shadow price
    Scenario2$ExogShockCOVID    <- BaseL$TemplMat      # Exogenous COVID shock


    # Grouping the scenarios information in a list
    MTinputsDummy$Scenario1     = Scenario1
    MTinputsDummy$Scenario2     = Scenario2

    return(MTinputsDummy)

}


