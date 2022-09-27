
# rm(list=ls())

# Loading the script with all pre-requisites
source('integration/Dmat_01Config.R')

# Creating a coverage base (country, sector, fuel, years) that will be used in all functions
# This will also be a list of base structures to use as templates in all functions (Provides matrix size/coverage)
BaseList  <- DefineCoverage() # Defaults for country and time coverage

# This list is saved, as it will be later on read by the core functions
# write_rds(BaseList, 'integration/BaseList.RDS')


# Historic data on energy consumption, adapted to desired format
D         <- PrepHistEC(BaseL = BaseList)

# Data on existing policies (CP-related levels and coverages; Price control coefficients)
D         <- PrepExistPol(DD    = D,
                          BaseL = BaseList)
# Data on Emission Factors
D         <- PrepareEF(DD     = D,
                       BaseL  = BaseList)

# Data on historic domestic prices
D         <- PrepHistDomPrices(DD = D,
                               BaseL = BaseList)

# Data on elasticities
D         <- PrepElasticities(DD = D,
                              BaseL = BaseList,
                              LU    = BaseList$LU)

# Emissions (Computed as in CPAT, with EC and EF)
D         <- PrepEmissions(DD = D,
                           BaseL = BaseList,
                           LU    = BaseList$LU)

# Storing the results to be read by the CPAT test scripts
saveRDS(D, 'R/DforIntegration.RDS')

