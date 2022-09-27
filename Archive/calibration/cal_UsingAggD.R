
# rm(list=ls())

# Initialize working directory to the position of this file:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(magrittr)
library(readxl)
library(dplyr)
library(ggplo2)

D_aggSG       <- readRDS('DforCalibAggSG') %>%
                  select(ID, ec, ec.obs) %>%
                  separate(ID, into = c('CountryCode', 'SectorGroup', 'FuelType')) %>%
                  # Excluding China from the analysis
                  filter(CountryCode != 'CHN')


# Initializing a D-matrix type of object for the resulting data.
# Only the column with unique SectorGroup is needed:
D_sg_sum      <- D_aggSG %>%
                  select(SectorGroup) %>%
                  distinct()

# We have n columns. Specify here the position of the columns where the data matrices are:
pos_consider  <- which(names(D_aggSG) %in% c('ec','ec.obs'))


# Extracting the matrices so that we can apply summarise() from dplyr
for(j in pos_consider){ # j <- 4


  # The first line will break the structure of matrix column and provide a df with years on the columns
  # if the double bracket is not used, the matrix column remains as a single col.
  # We only want to aggregate by SectorGroup, so we extract that column along with the data matrix
  temp_df           <- tibble(cbind('SectorGroup' = D_aggSG$SectorGroup ,as.data.frame( D_aggSG[[ j ]] ) )) %>%
                        group_by(SectorGroup) %>%
                        summarise(across(where(is.numeric), sum))

  # To return to the structure where the data matrix is stored in a single column:
  temp_tib          <- tibble('SectorGroup'         = temp_df$SectorGroup,
                              # Assigning names in dynamic form:
                              '{names(D_aggSG)[j]}' := as.matrix(temp_df[,-c(1:3)]))  # excluding first 3 columns from potential names

  # Intermediate matrix over which we will append by column the other matrix columns
  D_sg_sum          <- D_sg_sum %>%
                        left_join(temp_tib, by = 'SectorGroup')

}


# This new object should have 3 columns, with the latter 2 being ec and ec.obs
# For any filtering (i.e if only 'pow', 'res' and 'ind' are needed), the dplyr operations would work without problem
Test_D              <- D_sg_sum %>%
                          filter(SectorGroup %in% c('pow', 'ind', 'res'))

#Resid_ec            <- tibble(cbind('SectorGroup' = Test_D$SectorGroup, as.data.frame( Test_D$ec / Test_D$ec.obs  ) )) %>%
t                   <- 1:28

Resid_ec            <- tibble(cbind('SectorGroup' = Test_D$SectorGroup, as.data.frame((Test_D$ec/Test_D$ec.obs)^(1/t)))) %>%
                        pivot_longer(cols = c('1992':'2019'), names_to = 'Year', values_to = 'ResidEC')

# Plot
Resid_ec %>%
  ggplot(aes(x=Year, y=ResidEC)) +
  facet_grid(SectorGroup ~ ., scales = 'free') +
  geom_line(color="grey", group=1) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  labs(title = "Residuals' behaviour across time in the different sectors",
       y = "", x = "")

