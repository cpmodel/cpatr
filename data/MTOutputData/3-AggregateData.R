
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
library(openxlsx)
#EXAMPLE 1: SIMPLE EXTRACT OF DATA###############################################################################
MTSelectedOutput <- read_rds(paste0('data/MTOutputData/2_Processed/Latest_Long_All_Indicators.rds')) %>%
  filter(Variable %in% c("elec","ener","emis"))

MTSelectedAggregatedToWorld = MTSelectedOutput %>% mutate(GroupByCounter=1) %>%
  group_by(Scenario,Purpose,Variable,CPATIndicator,RunDate,Sector,FuelType,Year,SubScenario) %>%
  summarise(Value=sum(Value,na.rm=T), GroupByCounter=sum(GroupByCounter,na.rm=T))

saveRDS(MTSelectedAggregatedToWorld,paste0('data/MTOutputData/2_Processed/World_Latest_Long_All_Indicators.rds'))

MTSelectedAggregatedToWorld.TS = MTSelectedAggregatedToWorld %>% pivot_wider(names_from = Year, values_from = Value)
View(MTSelectedAggregatedToWorld.TS)
write.xlsx(MTSelectedAggregatedToWorld.TS,paste0('data/MTOutputData/2_Processed/World_Latest_TS_All_Indicators.xlsx'))
