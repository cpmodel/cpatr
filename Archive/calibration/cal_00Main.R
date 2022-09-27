library(tidyverse)
library(magrittr)
library(readxl)
library(dplyr)

# rm(list = ls())



source('calibration/cal_01DataPreparationFunctions.R')


# Arranging the data (script format) into a single D matrix

EC.obs.list     <- PrepareObsEC(InitYear = 2000,
                                FinYear  = 2017)

EC.hind.list    <- PrepareHindEC(InitYear = 2000,
                                 FinYear  = 2017)

GDP.list        <- ComputeGDPtoBase(InitYear  = 2000,
                                    FinYear   = 2017,
                                    BaseY     = 2000)

# Maximum number of dimensions:
# Countries
c_ECobs     <- unique(EC.obs.list$s$CountryCode)
c_EChind    <- unique(EC.hind.list$s$CountryCode)
CountryList <- intersect(c_ECobs, c_EChind)

# SectorGroup
g_ECobs         <- unique(EC.obs.list$g$SectorGroup)
g_EChind        <- unique(EC.hind.list$g$SectorGroup)
SectorGroupList <- intersect(g_ECobs, g_EChind)

# SectorCode
s_ECobs         <- unique(EC.obs.list$s$SectorCode)
s_EChind        <- unique(EC.hind.list$s$SectorCode)
SectorCodeList  <- intersect(s_ECobs, s_EChind)

# Fuel
f_ECobs     <- unique(EC.obs.list$s$FuelCode)
f_EChind    <- unique(EC.hind.list$s$FuelCode)
FuelList    <- intersect(f_ECobs, f_EChind)

# Creating all possible combinations to determine the rows of the D matrix at the sector group level:
IDcols      <- expand.grid('CountryCode' = CountryList,
                           'SectorGroup'  = SectorGroupList,
                           'FuelCode'    = FuelList)


D_cal.sg    <- IDcols %>%
                left_join(EC.obs.list$g, by = c('CountryCode', 'SectorGroup', 'FuelCode')) %>%
                left_join(EC.hind.list$g, by = c('CountryCode', 'SectorGroup', 'FuelCode'))


# Including the list of GDP_related variables
for(j in c(1:length(GDP.list))){
  D_cal.sg  <- D_cal.sg %>%
                left_join(GDP.list[[j]], by = 'CountryCode')
}

saveRDS(D_cal.sg, 'calibration/D.sg_latest')
