rm(list = ls(),envir = .GlobalEnv)

library(dplyr)
library(tibble)
library(tidyr)
library(stringr)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(magrittr)
library(readxl)
library(ggplot2)
library(dplyr)
library(hrbrthemes)


D           <- readRDS('DforCalibration')



temp <- D %>% filter(CountryCode %in% c('USA', 'BRA'),
             FuelType == 'coa',
             SectorCode == 'pow')


# temp matrix has countries in the rows and years in the columns (country x year)
# Sum by year for all countries
colSums(temp$ec)

# Sum by country for all years
rowSums(temp$ec)

colSums(D$ec.obs, na.rm = T)

resid.dat   <- temp$ec - temp$ec.obs


colSums(resid.dat, na.rm = T)

resid.id    <- D %>%
                select(CountryCode,SectorCode, FuelType)

resid       <- tibble(resid.id, resid.dat)

# resid.dat   <- (D$ec - D$ec.obs)^2
#
# resid.id    <- D %>%
#                 select(CountryCode,SectorCode, FuelType)
#
# resid       <- tibble(resid.id, resid.dat)
# Testing if residuals appear after 2000
temp %>% filter(CountryCode %in% c('USA', 'BRA', 'ZAF','COL','ECU'),
                          FuelType == 'coa',
                          SectorCode == 'pow') %>%
          select(resid.dat)

#
# D %>% filter(SectorCode == 'pow',
#              FuelType == 'coa',
#              CountryCode %in% c('USA', 'BRA', 'ZAF','COL','ECU')) %>%
#       select(CountryCode, SectorCode, FuelType, ec.obs)

# Weighted mean residuals
  # 1. Need to aggregate the energy consumption for each year (sum of all energy consumption for all countries per year for ec and ec.obs)
  # 2. We end up with two vectors: ec and ec.obs, aggregated at the world level
  # 3. Need to extract new vectors for ec and ec.obs
       # Actual (ec.obs): 2000 to 2018
       # Predicted (ec): 2001 to 2019
  # 4. Need to compute the weighted residual mean: (Actual/Predicted)^(1/19) - 1


##################################################################################################################################################
################################################### I - Residuals: observed vs. projected data ###################################################
##################################################################################################################################################

# 1. Overall average: Need to aggregate the energy consumption for each year (sum of all energy consumption for all countries per year for ec and ec.obs)
  # Two vectors: ec and ec.obs, aggregated at the world level
  ec_w       <- colSums(D$ec, na.rm = T)
  ec_w_obs   <- colSums(D$ec.obs, na.rm = T)

# 2. Need to extract new vectors for ec and ec.obs
  ec_w_red       <- ec_w[-(1:1)]  # Predicted (ec): 1991 to 2019
  ec_w_obs_red   <- ec_w_obs[-(30:30)]  # Actual (ec.obs): 1990 to 2018

# 3. Overall mean across time
  t_lag     <- 1:29 #time indicator
  Alpha_lag <- (ec_w_obs_red/ec_w_red)^(1/t_lag) - 1 # Overall mean = time trend

  Year_lag  <- 1991:2019

  df_lag    <- data.frame(Year_lag, Alpha_lag)

  t         <- 1:30
  Alpha     <- (ec_w_obs/ec_w)^(1/t) - 1 # Overall mean = time trend

  Year      <- 1990:2019

  df        <- data.frame(Year, Alpha)

# 4. Plot
df %>%
  ggplot(aes(x=Year, y=Alpha)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Autonomous Efficiency Improvement") +
  geom_hline(aes(yintercept = 0))

df_lag %>%
  ggplot(aes(x=Year_lag, y=Alpha_lag)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Autonomous Efficiency Improvement (lagged)") +
  geom_hline(aes(yintercept = 0)) +
  xlab("Year")

##################################################################################################################################################
################################################### # II - Regression: fit outside of the sample #################################################
##################################################################################################################################################
