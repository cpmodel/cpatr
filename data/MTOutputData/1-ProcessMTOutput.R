#-------------------------------------------------------------------------------
# Name:     MT_MTOutputToLong.R
# Purpose:  Transform MTOutput to long/tidy version

# Change Log
# Version   Initials    Date        Change
# 0.1       DB          12-Apr-22   Creation
# 0.2       FG/SS       20-May-22   Modification and finalisation
#-------------------------------------------------------------------------------


rm(list=ls())

library(dplyr)
library(tidyverse)
library(magrittr)
library(readxl)
library(openxlsx)

MTFileNamePrefix      <- 'MTO_'
MTFileNameSuffix      <- '_WB_Latest'
# Vector containing the sequence of years included in MTOutput
AllYears        <- as.character(2019:2036)

################################################################################

# Change here in to relate to the full list of scenarios
MTScenarioConfig = list(list(MTScenarioType='T', ScenarioNumbers=0:6,ScenarioSuffix=c("","t")))
#Made up example for 'HC' scenario with HC1 and HC2
#MTScenarioConfig = list(list(MTScenarioType='T',ScenarioNumbers=0:6,ScenarioSuffix=c("","t")),
#                        list(MTScenarioType='HC',ScenarioNumbers=1:2,ScenarioSuffix="") #,Etc
#                                                                     )

#Here what variables to create special variable specific dataset for
VariablesToInclude <- c('elec','emis','ener')

# Date in which the run was performed
DateToAdd       <- "16-05-2022"



PurposeDef      <- 'Calibration, forecasts'
CreateExcels <- TRUE #it's slow, but set to TRUE to do it all!
################################################################################


RawMTOutput=list()
counter=1

#Across all different scenario types (eg T, HC, PS etc.)
for(i in 1:length(MTScenarioConfig)) {

  #Across all numbers in that scenario type
  for(j in MTScenarioConfig[[i]]$ScenarioNumbers) {

    #Across all scenario suffixes
    for(k in 1:length(MTScenarioConfig[[i]]$ScenarioSuffix)) {
      print(paste0(MTScenarioConfig[[i]]$MTScenarioType,"-",j,MTScenarioConfig[[i]]$ScenarioSuffix[k]))
      MTFileName = paste0(MTFileNamePrefix,MTScenarioConfig[[i]]$MTScenarioType,j,MTScenarioConfig[[i]]$ScenarioSuffix[k],MTFileNameSuffix)

      # MT Output after deleting "Dist", renaming columns and transforming it into long format
      RawMTOutputTemp     <- read_excel(paste0('data/MTOutputData/1_Raw/',MTFileName,'.xlsx')) %>%
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

      RawMTOutput[[counter]]=RawMTOutputTemp
      counter=counter+1
    }

  }
  CombinedMTOutput=bind_rows(RawMTOutput)

}
#Save the file as a big long rds with all indicators
print("StartingFileSave")
FilePath=paste0('data/MTOutputData/2_Processed/','Latest_Long_All_Indicators_All_Columns') #Comprehensive
saveRDS(CombinedMTOutput, paste0(FilePath,'.rds') )

print("FileSaveSelectedColumnVersion")
CombinedMTOutput %<>% select(Scenario,Purpose,Variable,CPATIndicator,RunDate,CountryCode,Sector,FuelType,Year,SubScenario,Value)
FilePath=paste0('data/MTOutputData/2_Processed/','Latest_Long_All_Indicators')
saveRDS(CombinedMTOutput, paste0(FilePath,'.rds') )

print("FileSaveSelectedColumnSelectedIndicatorsVersion")
CombinedMTOutput %<>% filter(Variable%in%VariablesToInclude)
FilePath=paste0('data/MTOutputData/2_Processed/','Latest_Long_Selected_Indicators')
saveRDS(CombinedMTOutput, paste0(FilePath,'.rds') )


#Now create RDS long and Excel time series for each. This takes about 5 min mostly due to the Excel bit.

for(VariableUsed in VariablesToInclude) {
  FilePath=paste0('data/MTOutputData/2_Processed/','Latest_Long_',VariableUsed)
  TempTable= CombinedMTOutput %>%
    filter(Variable==VariableUsed)
    saveRDS(TempTable, paste0(FilePath,'.rds') )

    print(paste0("Saved-",VariableUsed,"-RDS"))
}

if(CreateExcels) {
for(VariableUsed in VariablesToInclude) {
  FilePath=paste0('data/MTOutputData/2_Processed/','Latest_Long_',VariableUsed)
  TempTable= CombinedMTOutput %>%
    filter(Variable==VariableUsed)

  #This takes a while: comment- in the next paths but then go and get a coffee!

    TempTableTS = TempTable %>%pivot_wider(values_from=Value,names_from=Year)
    FilePath=paste0('data/MTOutputData/2_Processed/','Latest_TimeSeries_',VariableUsed)
    write.xlsx(TempTableTS, paste0(FilePath,'.xlsx') )
    print(paste0("Saved-",VariableUsed,"-Excel"))
  }
}
#############################################################

