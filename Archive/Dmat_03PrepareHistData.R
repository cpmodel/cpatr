#---------------------------------------------------------------------------------
# Purpose:  Prepare all input and intermediate data for forecasting prices and EC.
#---------------------------------------------------------------------------------


#######################################
##    HISTORIC ENERGY CONSUMPTION    ##
#######################################

#  This function:
#   - Imports and arranges EC data for baseline year
#   - Calls the general function to expand data to account for all fuels and sectors
#   - Produces and reports a single column matrix to be used as input for forecasting EC

# NOTE:
# This function is the first to be applied, so there is no pre-existing D matrix, only the desired format.
# This function reads the elements of the desired format, and produces the first D matrix

PrepHistEC      <- function(BaseL = DefineCoverage()){  # List with structure and information of desired coverage (country, sector, fuel, years)

    # Reading the user-defined parameters that will provide the size of the matrices
    IDcols        <- BaseL$IDcols
    AllYears      <- BaseL$AllYears
    TemplMat      <- BaseL$TemplMat

    # Number of historical years to consider
    BackY         <- BaseL$BackYears

    HistEC        <- read_csv("data/EnergyConsumption/EnergyConsumption_1970_to_2019.csv")  %>%
                      rename('CountryCode' = country_code,
                             'Year' = year,
                             'elec' = ele) %>%
                      mutate(Variable = 'enc',
                             Year = as.character(Year)) %>%
                      select(-elec, -neu, -wav) %>% # elec is in Gwh
                      # Note the column selection is done manually stating the starting and final column (pow:oen)
                      # We could alternatively use where(is.numeric), but sometimes other columns are read as numeric as well
                      pivot_longer(names_to = "SectorCode", values_to = "Value", cols = pow:oen) %>%
                      select(CountryCode:Value) %>%
                      filter(Year %in% AllYears) %>%
                      pivot_wider(names_from = Year, values_from = Value) %>%
                      # Dropping 'ele' as fuel and renaming the FuelType as FuelCode
                      # TBC: Should we reintroduce ele? (FuelType vs FuelCode)
                      mutate(FuelType = replace(FuelType, FuelType == 'ecy', 'ele')) %>%
                      #filter(FuelType != 'ele') %>%
                      rename('FuelCode' = FuelType)


    # Expanded data on historical energy consumption
    ExpEC         <- IDcols %>%
                      left_join(HistEC, by = c('CountryCode', 'SectorCode', 'FuelCode')) %>%
                      # Overwritting the Variable column, as it will create NAs whenever there is no data
                      mutate(Variable = 'enc')

    # NOTE:
    # The historic data on 'enc' comes from IEA. For 2019, we also have Enerdata's info. Both used to create CPAT's projections.
    # 2019 data is replaced with CPAT's projections to increase the data coverage in the D matrix for this variable/year
    # TBC: make this dynamic for the latest year available
    CPATProjEC    <- read_csv("data/EnergyConsumption/EC_OutputForCPAT_2019_LATEST.csv")  %>%
                      rename(CPATCode = 'CPAT Code') %>%
                      separate(CPATCode, into = c("CountryCode", "Tab", "Variable", NA)) %>%
                      select(-CountryName, -Year, -Tab, -ely, -neu, -wav) %>%
                      mutate(CountryCode = toupper(CountryCode)) %>%
                      pivot_longer(names_to = "SectorCode", values_to = "Value", cols = pow:oen) %>%
                      pivot_wider(names_from = "Variable", values_from = "Value") %>%
                      mutate(FuelType = replace(FuelType, FuelType == 'ecy', 'ele')) %>%
                      # filter(FuelType != 'ecy') %>%
                      # Dropping 'ecy' as fuel and renaming the FuelType as FuelCode
                      rename('FuelCode' = FuelType)

    ExpCPATProj   <- IDcols %>%
                      left_join(CPATProjEC, by = c('CountryCode', 'SectorCode', 'FuelCode')) %>%
                      # Overwritting the Variable column, as it will create NAs whenever there is no data
                      mutate(Variable = 'enc')

    # Filling the matrix to be returned
    EC.matrix                 <- TemplMat
    EC.matrix[,c(1:BackY)]    <- as.matrix(ExpEC %>% select(-c(names(IDcols),'Variable')))

    # Overwritting the last year of historical data with CPAT's projections
    EC.matrix[,BackY]         <- as.matrix(ExpCPATProj %>% select(-c(names(IDcols),'Variable')))


    # Replacing NAs with 0:
    EC.matrix                 <- as.matrix(coalesce(as.data.frame(EC.matrix) %>% mutate_all(as.numeric),
                                                    as.data.frame(TemplMat)))

    # Creating the matrix-column
    EC                <- tibble(IDcols,
                                'ec' = EC.matrix)

    return(EC)
}



######################################################################
##    EXISTING POLICIES: CARBON TAX AND ETS - LEVEL AND COVERAGE    ##
######################################################################

# The function reads existing policies levels and coverage for carbon-price-related policies
# It also reads required coefficients related to price controls

PrepExistPol      <- function(DD    = D,
                              BaseL = BaseList,
                              LU    = BaseList$LU){

    # NOTE: TBC
    # The expansion process is not the same for the national price, the fuel coverage and the sector coverage.
    # As a result, the blocks of code are similar but not equal to each other. To consider when updating/modifying.

    IDcols        <- BaseL$IDcols
    AllYears      <- BaseL$AllYears
    TemplMat      <- BaseL$TemplMat

    # Number of historical years to consider
    BackY         <- BaseL$BackYears

    # List with data on existing policies and coverage
    RawExistPol            <- list()
    ExistPol               <- tibble(IDcols)

    #######################
    # Existing Carbon Tax #
    #######################

    #-------------------------------------------------------------------
    # XCT - Existing carbon tax (Nation-wide price, USD per ton of CO2e)
    #-------------------------------------------------------------------

    RawExistPol$XCT     <- read_excel("data/ExistingCT_ETS/ExistingCT_ETS.xlsx", sheet = "ExistingCT") %>%
                              rename('CountryCode' = countrycode) %>%
                              select(CountryCode, where(is.numeric))

    # Expanding National price for all fuels and sectors

    # Auxiliary matrix covering only the years for which there is data
    AuxMatShort           <- as.matrix(IDcols %>%
                                         left_join(RawExistPol$XCT, by = 'CountryCode') %>% select(any_of(AllYears)))

    # Auxiliary matrix including all years (Template matrix)
    AuxMat                <- TemplMat
    ColsToWrite           <- which(colnames(AuxMatShort) %in% colnames(AuxMat))
    AuxMat[,ColsToWrite]  <- AuxMatShort
    AuxMat[is.na(AuxMat)] <- 0

    # Writing this to a tibble:
    ExistPol              <- tibble(ExistPol, 'XCT' =  AuxMat)

    #-----------------------
    # XCT - Coverage by fuel
    #-----------------------

    RawExistPol$XCTcov_f  <- read_excel("data/ExistingCT_ETS/ExistingCT_ETS.xlsx", sheet = "ExistingCTFuelCover") %>%
                              rename('CountryCode' = countrycode,
                                     'FuelCode' = `sector/fuel`) %>%
                              select(CountryCode, FuelCode, where(is.numeric))

    # Auxiliary matrix covering only the years for which there is data
    AuxMatShort           <- as.matrix(IDcols %>%
                                         left_join(RawExistPol$XCTcov_f, by = c('CountryCode', 'FuelCode')) %>%
                                         select(any_of(AllYears)))

    # Auxiliary matrix including all years (Template matrix)
    AuxMat                <- TemplMat
    ColsToWrite           <- which(colnames(AuxMatShort) %in% colnames(AuxMat))
    AuxMat[,ColsToWrite]  <- AuxMatShort
    AuxMat[is.na(AuxMat)] <- 0


    # Writing this to a tibble:
    ExistPol              <- tibble(ExistPol,
                                    'XCTcov_f' = AuxMat)

    #-------------------------
    # XCT - Coverage by sector
    #-------------------------

    RawExistPol$XCTcov_s  <- read_excel("data/ExistingCT_ETS/ExistingCT_ETS.xlsx", sheet = "ExistingCTSectorCover") %>%
                              rename('CountryCode' = countrycode,
                                     'SectorGroup' = `sector/fuel`) %>%
                              select(CountryCode, SectorGroup, where(is.numeric)) %>%
                              # Correcting Transport sector code:
                              mutate(SectorGroup = replace(SectorGroup, SectorGroup == 'trs', 'tra')) %>%
                              # Expanding SectorGroup to SectorCode
                              left_join(LU$SectorGroups, by = 'SectorGroup') %>%
                              select(CountryCode, SectorCode, where(is.numeric))

    # Auxiliary matrix covering only the years for which there is data
    AuxMatShort           <- as.matrix(IDcols %>%
                                         left_join(RawExistPol$XCTcov_s, by = c('CountryCode', 'SectorCode')) %>%
                                         select(any_of(AllYears)))

    # Auxiliary matrix including all years (Template matrix)
    AuxMat                <- TemplMat
    ColsToWrite           <- which(colnames(AuxMatShort) %in% colnames(AuxMat))
    AuxMat[,ColsToWrite]  <- AuxMatShort
    AuxMat[is.na(AuxMat)] <- 0


    # Writing this to a tibble:
    ExistPol              <- tibble(ExistPol,
                                    'XCTcov_s' = AuxMat)


    #---------------------------------
    # CT - Coverage by sector and fuel
    #---------------------------------

    # Obtaining a single matrix detailing coverage at sector and fuel level per country

    ExistPol              <- tibble(ExistPol,
                                    'XCTcov_sf' = ExistPol$XCTcov_s * ExistPol$XCTcov_f)


    ########################
    # Existing ETS Permits #
    ########################

    #---------------------------------------------------------------------------
    # XETSP - Existing ETS permit price (Nation-wide price, USD per ton of CO2e)
    #---------------------------------------------------------------------------

    RawExistPol$XETSP   <- read_excel("data/ExistingCT_ETS/ExistingCT_ETS.xlsx", sheet = "ExistingETS") %>%
                              rename('RegionETS' = countrycode) %>%
                              left_join(LU$RegionETS, by = "RegionETS") %>%
                              mutate(CountryCode = coalesce(CountryCode, RegionETS)) %>%
                              select(CountryCode, where(is.numeric)) %>%
                              mutate_if(is.numeric, ~replace_na(., 0))

    # Expanding National price for all fuels and sectors

    # Auxiliary matrix covering only the years for which there is data
    AuxMatShort           <- as.matrix(IDcols %>%
                                         left_join(RawExistPol$XETSP, by = 'CountryCode') %>%
                                         select(any_of(AllYears)) %>%
                                         mutate_if(is.numeric, ~replace_na(., 0)) )

    # Auxiliary matrix including all years (Template matrix)
    # TBC: Consider here the option to have existing ETS growing at a country-specific rate
    AuxMat                <- TemplMat
    ColsToWrite           <- which(colnames(AuxMatShort) %in% colnames(AuxMat))
    AuxMat[,ColsToWrite]  <- AuxMatShort
    AuxMat[is.na(AuxMat)] <- 0

    # Writing this to a tibble:
    ExistPol              <- tibble(ExistPol, 'XETSP' =  AuxMat)

    #------------------------
    # XETS - Coverage by fuel
    #------------------------

    RawExistPol$XETScov_f <- read_excel("data/ExistingCT_ETS/ExistingCT_ETS.xlsx", sheet = "ExistingETSFuelCover") %>%
                              rename('CountryCode' = countrycode,
                                     'FuelCode' = `sector/fuel`) %>%
                              select(CountryCode, FuelCode, where(is.numeric))


    # Auxiliary matrix covering only the years for which there is data
    AuxMatShort           <- as.matrix(IDcols %>%
                                         left_join(RawExistPol$XETScov_f, by = c('CountryCode', 'FuelCode')) %>%
                                         select(any_of(AllYears)) %>%
                                         mutate_if(is.numeric, ~replace_na(., 0)) )

    # Auxiliary matrix including all years (Template matrix)
    AuxMat                <- TemplMat
    ColsToWrite           <- which(colnames(AuxMatShort) %in% colnames(AuxMat))
    AuxMat[,ColsToWrite]  <- AuxMatShort
    AuxMat[is.na(AuxMat)] <- 0


    # Writing this to a tibble:
    ExistPol              <- tibble(ExistPol,
                                    'XETScov_f' = AuxMat)

    #--------------------------
    # XETS - Coverage by sector
    #--------------------------

    RawExistPol$XETScov_s  <- read_excel("data/ExistingCT_ETS/ExistingCT_ETS.xlsx", sheet = "ExistingETSSectorCover") %>%
                                rename('CountryCode' = countrycode,
                                       'SectorGroup' = `sector/fuel`) %>%
                                select(CountryCode, SectorGroup, where(is.numeric)) %>%
                                # Correcting Transport sector code:
                                mutate(SectorGroup = replace(SectorGroup, SectorGroup == 'trs', 'tra')) %>%
                                # Expanding SectorGroup to SectorCode
                                left_join(LU$SectorGroups, by = 'SectorGroup') %>%
                                select(CountryCode, SectorCode, where(is.numeric))

    # Auxiliary matrix covering only the years for which there is data
    AuxMatShort           <- as.matrix(IDcols %>%
                                         left_join(RawExistPol$XETScov_s, by = c('CountryCode', 'SectorCode')) %>%
                                         select(any_of(AllYears)) %>%
                                         mutate_if(is.numeric, ~replace_na(., 0)) )

    # Auxiliary matrix including all years (Template matrix)
    AuxMat                <- TemplMat
    ColsToWrite           <- which(colnames(AuxMatShort) %in% colnames(AuxMat))
    AuxMat[,ColsToWrite]  <- AuxMatShort
    AuxMat[is.na(AuxMat)] <- 0


    # Writing this to a tibble:
    ExistPol              <- tibble(ExistPol,
                                    'XETScov_s' = AuxMat)


    #-----------------------------------
    # XETS - Coverage by sector and fuel
    #-----------------------------------

    # Obtaining a single matrix detailing coverage at sector and fuel level per country

    ExistPol              <- tibble(ExistPol,
                                    'XETScov_sf' = ExistPol$XETScov_s * ExistPol$XETScov_f)


    ##############################
    # PRICE CONTROL COEFFICIENTS #
    ##############################

    RawExistPol$PCtrl     <- read_excel("data/ExistingCT_ETS/ExistingCT_ETS.xlsx", sheet = "ExistingPriceControls") %>%
                              rename('CountryCode' = Countrycode) %>%
                              select(CountryCode, where(is.numeric), -ifscode) %>%
                              pivot_longer(-CountryCode, names_to = 'Variable', values_to = 'Value') %>%
                              separate(Variable, into = c('Indicator', 'FuelType', 'SectorGroup')) %>%
                              mutate(SectorGroup = replace(SectorGroup, SectorGroup == 'res', 'bld'),
                                     Indicator = paste0('PCtrl_',Indicator)) %>%
                              pivot_wider(names_from = 'Indicator', values_from = 'Value') %>%
                              # Expanding data at SectorCode and FuelCode levels:
                              left_join(LU$SectorGroups, by = 'SectorGroup') %>%
                              left_join(LU$FuelTypes, by = 'FuelType') %>%
                              select(CountryCode, SectorCode, FuelCode, where(is.numeric))



    #############################
    # CLEANING MATRIX TO RETURN #
    #############################


    ExistPol    <- ExistPol %>%
                    # Dropping redundant information
                    select(-c(XCTcov_s, XCTcov_f, XETScov_s, XETScov_f)) %>%
                    # Including Price control coefficients
                    left_join(RawExistPol$PCtrl, by = c('CountryCode', 'SectorCode', 'FuelCode'))

    # Including data into the D matrix format
    DD          <- DD %>%
                    left_join(ExistPol, by = c('CountryCode', 'SectorCode', 'FuelCode'))


    # Setting all NA in the recently added variables to 1
    # Floating part of the supply cost: 1 = no price control (full international price pass through)
    for( j in c('PCtrl_alpha0', 'PCtrl_deltabc')){
        tempRep                 <- DD[j]
        tempRep[is.na(tempRep)] <- 1
        DD[j]                   <- tempRep
    }

    # Returning D matrix with added data
    return(DD)

}


##############################
##     EMISSION FACTORS     ##
##############################

#  This function:
#   - Imports Emission Factors data
#   - Expands the EF information from SectorGroup to SectorCode
#   - Fills missing values according to rule read from the respective lookup table
#   - Completes information by adding fuels with 0 EFs

PrepareEF           <- function(DD    = D,
                                BaseL = BaseList,
                                LU    = BaseList$LU) {

    # Several steps to build the EF vector:
    # 1. T1: Expanding existing Sector Groups to Sector Codes in raw data
    # 2. T2: Dealing with SectorGroup 'all' (which are treated here as 'all other non specified elsewhere')
    # 3. T3: Overwriting missing values using EF data from other sector.fuel, according to rules specified in the lookups
    # 4. T4: Including fuels with assumed 0 emissions (excluded from EF raw table)
    # 5. T5: Appending all relevant subtables, and selecting columns to report

    # STEP 1: ------------------------------------------------------------------
    # Expanding existing groups:
    #       a) T0: Raw Imported Data for further use
    #       b) T1: Filter out 'all' from T0.
    #       c) T1: Join with lookup to expand SectorGroup into SectorCode.
    #       d) T1: Add back the rod & avi, which did not got dropped from T1
    #       e) T1: Create ID1 variable 'Country.Sector.Fuel'

    # Subset of map from Groups to sectors, excluding 'all' and the unused FlowCode
    EFSectorGroups  <- LU$SectorGroups %>%
                        filter(SectorGroup != 'all') %>%
                        select(-FlowCode)

    # Step 1, item a)
    # Importing raw table. (includes 'rod' and 'avi' among SectorGroups -> it already expands 'tra' into some SectorCode)
    T0              <- read_excel("data/EmissionFactors/EmissionFactorsCountryFuel.xlsx", sheet = "EmissionFactorsCountry") %>%
                        filter(Notes == "ef CO2") %>%   # Not needed given new form of input. Left in case data is unfiltered.
                        select(CountryCode, SectorCode, FuelType, ef) %>%
                        rename(SectorGroup = SectorCode)

    # T0 contains some sectors (e.g. 'avi' and 'rod') under the column SectorGroups.
    # This will be excluded when performing the inner_join in T1, so need to be included back afterwards.
    SectorsToAdd    <- unique(T0$SectorGroup) %>%
                        subset(!. %in% c('all',(intersect(unique(T0$SectorGroup), unique(EFSectorGroups$SectorGroup)))))


    # Table to append
    T0.add          <- T0[T0$SectorGroup %in% SectorsToAdd,] %>%
                        rename(SectorCode = SectorGroup)

    # Step 1, items b), c), d) and e)
    # Table resulting from Step 1:
    T1              <- T0 %>%
                        # Expanding Sector Groups into Sectors
                        # This join already excludes 'all', but also 'rod' and 'avi' from original table.
                        inner_join(EFSectorGroups, by = "SectorGroup") %>%
                        #rename(SectorCode = Sector) %>%
                        select(CountryCode, SectorCode, FuelType, ef) %>%
                        # Adding excluded sectors originally mislabled as SectorGroups (except 'all') from original table
                        rbind(T0.add) %>%
                        mutate('ID1' = paste(CountryCode,SectorCode,FuelType, sep = '.'))


    # STEP 2: ------------------------------------------------------------------
    # Dealing with 'all':
    #       a) T2.1:        Keep only Sector = 'all' from original table.
    #       b) T2.2:        Combine all expected countries, sectors and (non-zero EFs) fuel types.
    #       c) T2.2:        Create ID1 as in T1. Drop all rows that exist in T1.
    #       d) T2.1 & T2.2: Create ID2 column for 'country.fuel' (not sector)
    #       e) T2:          Join T2.1 and T2.2 by ID2. (Expand T2.1 to the required number of rows given by 'country.fuel' in T2.2)
    #       f) T2:          Drop ID2


    # Step 2, items a) and d)
    # Table with VALUES to be expanded to 'all' other sectors
    T2.1            <- T0 %>%
                        filter(SectorGroup == 'all') %>%
                        mutate('ID2' = paste(CountryCode,FuelType, sep = '.')) %>%
                        select(ID2, ef)


    # List of countries covered in EF database
    #CountriesEF     <- unique(T0$CountryCode)
    CountriesEF     <- unique(DD$CountryCode)

    # Vector of fuels with non-zero emission factors
    NonZeroFuels    <- unique(T0$FuelType) %>%
                        intersect(unique(LU$EFCompleting$Fuel))

    # Excluding 'neu' and 'wav' from all sectors (TBC: Desired?)
    SectorEF        <- unique(LU$SectorGroups$SectorCode) %>%
                        subset(!. %in% c('neu','wav'))

    # Step 2, items b), c) and d)
    # Table with list of 'all other sectors' over which to expand the values contained in T2.1
    T2.2            <- expand.grid('CountryCode' = CountriesEF, 'SectorCode' = SectorEF, 'FuelType' = NonZeroFuels) %>%
                        mutate('ID1' = paste(CountryCode, SectorCode, FuelType, sep = '.'),
                               'ID2' = paste(CountryCode, FuelType, sep = '.')) %>%
                        filter(! ID1 %in% T1$ID1 )

    # Step 2, item e) and f)
    # Table resulting from Step 2 (Expansion of 'all other sectors')
    T2              <- tibble(left_join(T2.2, T2.1, by = 'ID2')) %>%
                        select(- ID2) %>%
                        mutate('ID3' = paste(SectorCode,FuelType, sep = '.'))


    # T2 %>% filter(is.na(ef), SectorCode != 'elec') %>% select(SectorCode) %>% unique()

    # STEP 3: ------------------------------------------------------------------
    # Filling missing values of EF for the 'sector.fuel' according to rules defined in lookup table.
    #       a) T3.1:    Create table with rules on how to fill the missing data (at SectorGroup level --> Use T0 to fill it)
    #       b) T3.2:    Expand the table version of sectors to consider (from T3.1), include countries and source to fill value
    #       c) T3.3:    Use T0 (its at SectorGroup level), to create a table from where the values will be taken with an ID to match
    #       d) T3:      Final table filled with EF data from other sector.fuel according to rules in lookup table


    # Step 3, item a)
    # Create table with rules on how to fill the missing data (at SectorGroup level --> Use T0 to fill it)
    T3.1        <- LU$EFCompleting %>%
                    filter(!is.na(CompleteFrom)) %>%
                    mutate('ID3' = paste(SectorGroup,Fuel, sep = '.')) %>%
                    select(-SourceSectorGroupFuel) %>%
                    rename(SectorCode = SectorGroup)  # No groups contained in mapping rules

    # Step 3, item b)
    # Expand the table version of sectors to consider (from T3.1), include countries and source to fill value
    T3.2        <- expand.grid('CountryCode' = CountriesEF,
                               'ID3'         = T3.1$ID3) %>%
                    # Adding the ID at Sector level
                    mutate('ID1' = paste(CountryCode,ID3, sep = '.')) %>%
                    # Including the matching rules
                    left_join(T3.1, by = 'ID3') %>%
                    mutate('IDSource' = paste(CountryCode, CompleteFrom, sep = '.')) %>%
                    select(ID1, IDSource)

    # Step 3, item c)
    # Use T0 (its at SectorGroup level), to create a table from where the values will be taken with an ID to match
    T3.3        <- T0 %>%
                    mutate('IDSource' = paste(CountryCode, SectorGroup, FuelType, sep = '.')) %>%
                    select(IDSource, ef)

    # Step 3, item d)
    # Final table filled with EF data from other sector.fuel according to rules in lookup table
    T3          <- T3.2 %>%
                    left_join(T3.3, by = 'IDSource') %>%
                    select(- IDSource) %>%
                    separate(ID1, into = c('CountryCode', 'SectorCode', 'FuelType'), remove = FALSE  ) %>%
                    filter(!is.na(ef))


    # STEP 4: ------------------------------------------------------------------
    # Including other fuels with 0 EF:
    #       a) T4: Creating a table with combination of Countries, all other fuels and all sectors

    # List of all (14) fuels
    FuelList    <- unique(LU$FuelTypes$FuelCode)

    # List of fuels assigned a 0 EF
    ZeroFuels   <- FuelList %>%
                    subset(!. %in% NonZeroFuels)

    # Step 4, item a)
    # Creating a table with combination of Countries, all other fuels and all sectors
    T4          <- expand.grid('CountryCode' = CountriesEF,
                               'SectorCode'  = SectorEF,
                               'FuelType'    = ZeroFuels) %>%
                    mutate(ef  = 0,
                           ID1 = paste(CountryCode, SectorCode, FuelType, sep = '.'))


    # STEP 5: ------------------------------------------------------------------
    # Append and format the resulting table:
    #       a) Drop auxiliary columns from all sub-tables
    #       b) EF: append all subtables and arrange them

    # Step 5, item a)
    T1f         <- T1 %>%
                    mutate(Step = 'T1') %>%
                    select(CountryCode, SectorCode, FuelType, ef, ID1, Step) %>%
                    # T3 will overwrite this information
                    filter(!ID1 %in% unique(c(T3$ID1, T4$ID1)) )

    T2f         <- T2 %>%
                    mutate(Step = 'T2') %>%
                    select(CountryCode, SectorCode, FuelType, ef, ID1, Step) %>%
                    # No repeated from T1, and not in T3, as T3 will overwrite this information
                    filter(!ID1 %in% unique(c(T1f$ID1, T3$ID1, T4$ID1)) )

    # Note the existence of missing values despite the filling in
    T3f         <- T3 %>%
                    mutate(Step = 'T3') %>%
                    select(CountryCode, SectorCode, FuelType, ef, ID1, Step)

    T4f         <- T4 %>%
                    mutate(Step = 'T4') %>%
                    select(CountryCode, SectorCode, FuelType, ef, ID1, Step) %>%
                    filter(!ID1 %in% unique(c(T1f$ID1, T2f$ID1, T3f$ID1)))

    # Step 5, item b)
    # To avoid duplicates, we create an ID and only add non-previously-found entries from each new table:
    EF          <- rbind(T1f, T2f, T3f, T4f) %>%
                    rename('FuelCode' = FuelType) %>%
                    select(CountryCode, SectorCode, FuelCode, ef)

    # Check if duplicates appear in Step 5
    # EF %>% group_by(ID1) %>% mutate(count = n()) %>% arrange(ID1) %>% filter(count >1)

    # Including the EF data into the DD matrix.
    # NOTE: EF included as a vector. If required, it could be instead included as a time series with time-constant values
    DD          <- DD %>%
                    left_join(EF, by = c('CountryCode', 'SectorCode', 'FuelCode'))


    # Returning the DD matrix including the emission factors data
    return(DD)
}



####################################
##    HISTORIC DOMESTIC PRICES    ##
####################################

PrepHistDomPrices   <- function(DD    = D,
                                BaseL = BaseList,
                                LU    = BaseList$LU) {

    #------------------------------------------------------#
    # IMPORTING, CLEANING AND ARRANGIN DATA IN LONG FORMAT #
    #------------------------------------------------------#

    IDcols              <- BaseL$IDcols

    AllYears            <- BaseL$AllYears

    # Unlike the other inputs, historic prices are read for several years
    # We need to know what is the last historical year to consider, and how many years back are used as reference for some ratios
    FMYear              <- BaseL$FirstModYear
    BackY               <- BaseL$BackYears

    SelCountry          <- BaseL$SelCountry


    RawHistDomPrices    <- read_excel(paste0('data/Prices/1_Raw/prices_for_cpat_all_years_LATEST.xlsx')) %>%
                            # Dropping redundant variables
                            select(-c(country_year,
                                      countrycode_weo,
                                      starts_with('floating_'))) %>%
                            # Adopting CamelCase convention
                            rename(CountryCode = countrycode,
                                   Country = countryname,
                                   Year = year) %>%
                            # Keeping only the required years: (T0 - N):T0
                            filter(Year %in% c((FMYear-BackY):(FMYear-1)),
                                   CountryCode %in% SelCountry) %>%
                            # Ensuring columns with numeric data are read as such
                            mutate(across(.cols= starts_with("mit_"), .fns = as.numeric)) %>%
                            # Selecting the columns to keep. Numeric data is stored under columns named 'mit_...'
                            select(CountryCode, Country, Year, starts_with('mit_')) %>%
                            # Transforming to long format
                            pivot_longer(cols = starts_with('mit_'),  names_to = 'OldCPATCode', values_to = 'Value') %>%
                            # Extract relevant identificators from CPAT code
                            separate(OldCPATCode, into = c(NA, 'Indicator', 'FuelType', 'SectorGroup')) %>%
                            # Adopting documented notation:
                            mutate(Indicator = replace(Indicator, Indicator == 'vatrate', 'VATrate'),
                                   SectorGroup = replace(SectorGroup, SectorGroup == 'res', 'bld'))


    #------------------------------------------------#
    # EXPANDING DOMESTIC PRICES TO ALL SECTORS/FUELS #
    #------------------------------------------------#

    # vatrate is treated separately. While it is specified by sector/fuel in most cases. For some years this is not the case.
    # From the expansion, we will drop the general vatrate data, and add it back again whenever needed.
    Datavatrate         <- RawHistDomPrices %>%
                            filter(is.na(SectorGroup)) %>%
                            select(-c(Country, FuelType, SectorGroup))

    # This section expands sector categories to match CPAT's level of detail
    ExpandHistDomPr     <- RawHistDomPrices %>%
                            # Dropping the observations regarding vartare (separate treatment), and regarding source quality of sp and p
                            filter(! Indicator %in% c('VATrate', 'spsrcstr', 'rpsrcstr')) %>%
                            # Expanding data from SectorGroup to SectorCode
                            left_join(LU$SectorGroups, by = 'SectorGroup') %>%
                            # Renaming indicators to match latest conventions
                            mutate(Indicator = replace(Indicator, Indicator == 'prosun', 'ps'),
                                   Indicator = replace(Indicator, Indicator == 'txocp', 'txo'),
                                   Indicator = replace(Indicator, Indicator == 'rp', 'p') ) %>%
                            # Expanding data from FuelType to Fuel code (ecy prices assumed '=' for all generation types)
                            left_join(LU$FuelTypes, by = 'FuelType') %>%
                            # Selecting columns to keep
                            select(CountryCode, SectorCode, FuelCode, Year, Indicator, Value)

    # Creating a consistent dataset for VATrate to bind to the above table
    TableVATrate        <- ExpandHistDomPr %>%
                            select(CountryCode, SectorCode, FuelCode, Year) %>%
                            # Keeping only unique rows. Aside from what unique() does, distinct() also sorts the results
                            distinct() %>%
                            left_join(Datavatrate, by = c('CountryCode', 'Year'))

    # Binding the Tablevat with the expanded table for all other indicators
    ExpandHistDomPr     <- ExpandHistDomPr %>%
                            rbind(TableVATrate) %>%
                            arrange(CountryCode, Year, SectorCode, FuelCode)


    #-----------------------------------------------#
    # TRANSFORMING RESULTS INTO THE D-MATRIX FORMAT #
    #-----------------------------------------------#

    # Currently data covers only historical years. These will be transformed and loaded into the matrix covering also projected years.

    # Transforming into wide format (time series)
    WideHistDomPr       <- ExpandHistDomPr %>%
                            pivot_wider(names_from = Year, values_from = Value)


    # Filling the historical years of data for each indicator in matrix column form
    for(j in unique(WideHistDomPr$Indicator)){ # j <- 'sp'

        # Initializing a template matrix to be used for each indicator
        TemplMat             <- BaseL$TemplMat

        # The use of this auxiliar tibble is redundant, but it ensures a robust match of ID and data for each row.
        aux_tib         <- IDcols %>%
                            left_join(filter(WideHistDomPr, Indicator == j),
                                      by = c('CountryCode', 'SectorCode', 'FuelCode'))

        # Loading historical information in the first positions of the new matrix
        TemplMat[,1:BackY]      <- as.matrix(aux_tib %>%
                                    select(-c(CountryCode, SectorCode, FuelCode, Indicator)))

        # Matrix column for the current indicator
        MatCol          <- tibble(IDcols ,
                                  '{j}' := TemplMat)

        # Joining the current Indicator to the entire DD matrix (column)
        DD              <- DD %>%
                            left_join(MatCol, by = c('CountryCode', 'SectorCode', 'FuelCode'))
    }


    # ADJUSTMENT: Overwritting consumer side subsidy
    # Convention: cs is recorded in CPAT as the negative of the value reported in the IMF dataset
    DD$cs               <- -DD$cs


    #---------------------------------------------------------------#
    # DISAGGREGATION OF EXCISE AND OTHER TAXES FOR HISTORICAL YEARS #
    #---------------------------------------------------------------#

    # Initializing again (out of the loop) a template matrix to be used for each indicator
    TemplMat            <- BaseL$TemplMat
    #  matrix(NA, nrow = nrow(IDcols), ncol = length(YearsVector) )

    # Existing (Effective) Carbon Tax at fuel sector level (per energy unit)
    DD$xct              <- DD$XCT * DD$ef * DD$XCTcov_sf

    # Existing ETS permit price
    # CHECK: Excel takes into account up to 2022, here, it takes up to 2021 (First year of calculations)
    DD$xetsp            <- DD$XETSP * DD$ef * DD$XETScov_sf

    # Taxes fixed, Ad Valorem, others
    # Historic process is different than that for projections
    # DD$fao              <- DD$p %-% DD$sp %-% DD$vat %-% DD$xct %-% (
    #                         (DD$txo < DD$xetsp)*0 %+% (!DD$txo < DD$xetsp) %x% DD$xetsp ) %-% DD$cs

    DD$fao              <- (DD$p - DD$sp - DD$vat - DD$xct -
                              ((DD$txo < DD$xetsp)*0 + (!DD$txo < DD$xetsp) * DD$xetsp ) - DD$cs)

    # New excise tax (if applicable)
    DD$nexc             <- BaseL$TemplMat
    # Will be later replaced with manual inputs if desired
    DD$nexc[,1:BackY]   <- 0

    # New carbon Tax
    # Policies are implemented after the first year of simulations, so the trajectory here = 0
    DD$nct              <- BaseL$TemplMat
    DD$nct[,1:BackY]    <- 0

    # New ETS permit price
    # Policies are implemented after the first year of simulations, so the trajectory here = 0
    DD$netsp           <- BaseL$TemplMat
    DD$netsp[,1:BackY] <- 0



    #-------------------------------------------------------------#
    # COMPUTING HELPERS/CONSTANTS USED IN FORECASTING AND RESULTS #
    #-------------------------------------------------------------#

    # NOTE:
    #   - The averages and constants in here will be computed for all years between: (FMYear-BackY):(FMYear-1)
    #   - Operations can be affected by NA. Options have been included in each case not to consider NA in the calculation.
    #   - Despite this, the helpers may produce NaN as results. TBC: Introduce replacements downstream


    # The Helpers can be set here, but Helper4 requires international prices to be computed.
    # I will move all helpers to IP, which comes after DD is obtained

    # Helper1: Average retail price / Average supply cost (pre-tax price)
    H1_p_sp             <- rowMeans(DD$p, na.rm = T)/rowMeans(DD$sp[,c(1:BackY)], na.rm = T)

    # Helper2: Adjustment term. Floating wrt to international prices
    # TBC: This may need to be computed afterwards, as it depends on user assumptions
    Bucketed            <- FALSE
    FloatSupplyCost     <- Bucketed*DD$PCtrl_deltabc + (1 - Bucketed)*DD$PCtrl_alpha0
    H2_FLsp             <- (1 - FloatSupplyCost)*rowMeans(DD$sp[,c(1:BackY)], na.rm = T)


    # Helper3: Avg VAT payment / Avg pre-tax prices (sp)
    H3_vat_sp           <- rowMeans(DD$vat[,c(1:BackY)], na.rm = T)/rowMeans(DD$sp[,c(1:BackY)], na.rm = T)

    HelpersDD           <- tibble(IDcols,
                                  'H1_rp_sp'  = H1_p_sp,
                                  'H2_FLsp'   = H2_FLsp,
                                  'H3_vat_sp' = H3_vat_sp)


    DD                  <- DD %>%
                            left_join(HelpersDD, by = c('CountryCode', 'SectorCode', 'FuelCode'))

    return(DD)

}




########################
##    ELASTICITIES    ##
########################

PrepElasticities        <- function(DD    = D,
                                    BaseL = BaseList,
                                    LU    = BaseList$LU){


    # Reading the file with all types of elasticities (except cross-price) in a single table
    RawElasticities     <- read_excel("data/Elasticities/Elasticities.xlsx", sheet = "Elasticities") %>%
                            # Expand sector groups to sector codes, and fuel codes
                            left_join(LU$SectorElast , by = 'SectorGroup') %>%
                            left_join(LU$FuelElast , by = 'FuelGroup') %>%
                            # Drop un-needed columns
                            select(-c(Elasticities, Fuel, FuelGroup, Sector, SectorGroup)) %>%
                            pivot_longer(-c(Type, FuelCode, SectorCode), names_to = 'CountryCode', values_to = 'Value') %>%
                            select(CountryCode, SectorCode, FuelCode, Type, Value) %>%
                            pivot_wider(names_from = 'Type', values_from = 'Value') %>%
                            rename('ElasticityIncome' = Income,
                                   'ElasticityOwnPriceUsage' = 'Own-price, usage',
                                   'ElasticityOwnPriceEffic' = 'Own-price, efficiency')



    # Including the data into the main matrix
    DD      <- DD %>%
                left_join(RawElasticities, by = c('CountryCode', 'SectorCode', 'FuelCode'))



    # ------------------------------------- #
    # DUMMY PROCESS TO FILL IN MISSING DATA #
    # ------------------------------------- #

    DD      <- DD %>%
                mutate(ElasticityIncome        = coalesce(ElasticityIncome, 0),
                       ElasticityOwnPriceUsage = coalesce(ElasticityOwnPriceUsage, -0.01),
                       ElasticityOwnPriceEffic = coalesce(ElasticityOwnPriceEffic, -0.01))

    # ------------------------------------- #



    # Autonomous efficiency improvements in energy-consuming capital
    RawEffImprove     <- read_excel("data/Elasticities/Elasticities.xlsx", sheet = "EfficiencyImprovements") %>%
                          pivot_longer(-FuelCode, names_to = 'SectorCode', values_to = 'EfficImprove')

    EfficImpr         <- BaseL$IDcols %>%
                          left_join(RawEffImprove, by = c('SectorCode', 'FuelCode')) %>%
                          # Replacing missing values by 0:
                          mutate(EfficImprove = coalesce(EfficImprove, 0))


    DD      <- DD %>%
                left_join(EfficImpr, by = c('CountryCode', 'SectorCode', 'FuelCode'))


    return(DD)
}


#########################
##    CO2 EMISSIONS    ##
#########################

PrepEmissions           <- function(DD        = D,
                                    BaseL     = BaseList,
                                    LU        = BaseList$LU,
                                    AdjFactor = 1.12){

      DD$em       <- DD$ec * DD$ef * AdjFactor

      return(DD)
}
  #--------------------------------------------------------------------------
#
#
#
#
#
#
#
#
#
#     # Importing dummy data for prices from CPAT
#   PricesInCPAT    <- read_excel("data/Prices/PricesInCPAT.xlsx", sheet = "RetailPricesCPAT") %>%
#     rename(CPATCode = `CPAT Code`)
#
#
#
#   # The expansion of 'all' sectors is done afterwards
#   TempPrices      <- PricesInCPAT %>%
#     # Separating the CPATCode into sub codes
#     separate(col = "CPATCode", into = c("CountryCode", "Tab",
#                                         "Variable","FuelType",
#                                         "SectorCode", "Subscenario"), sep = "[.]") %>%
#     rename(SectorGroup = SectorCode) %>%
#     # Expanding SectorGroups to Sectors: Duplicating the values for all sectors in the relevant SectorGroup
#     inner_join(Mapping$SectorGroups, by = 'SectorGroup') %>%
#     select(-SectorGroup) %>%
#     # rename(SectorCode = Sector) %>%
#     mutate(CountryCode = toupper(CountryCode)) %>%
#     select(CountryCode, FuelType, SectorCode, as.character(InitYear:FinYear))
#
#   Prices.ID       <- TempPrices %>%
#     select(CountryCode,FuelType,SectorCode)
#
#   Prices.Val.Mat  <- TempPrices %>%
#     select(as.character(InitYear:FinYear)) %>%
#     as.matrix()
#
#   # Currently no NAs are generated:
#   Prices.Val.Mat[is.na(Prices.Val.Mat)]   <- 0
#
#   # Recording values in the desired output format
#   Prices          <- tibble(Prices.ID, rp = Prices.Val.Mat)
#
#   # TBC: Add prices for wnd sol hyd ore nuc = 1 as placeholder
#
#   return(Prices)
# }






# ##########################
# ##     ELASTICITIES     ##
# ##########################
#
# #  This function:
# #   - Imports data on Elasticities
# #   - Expands data (replicates) for all countries
# #   - TBC: To be updated, given the changes in the elasticities tab in CPAT
# PrepareElasticities     <- function(DD    = D,
#                                     BaseL = BaseList,
#                                     LU    = BaseList$LU){
#
#   Elasticities        <- read_excel("data/Elasticities/Elasticities.xlsx") %>%
#                           rename(SectorGroup = Sector) %>%
#                           filter(FuelType %in% FuelList) %>%
#                           inner_join(Mapping$SectorGroup, by = 'SectorGroup') %>%
#                           select(SectorCode, FuelType, el_inc, el_dem, el_cons, eff) %>%
#                           merge(ListOfCountries) %>%
#                           rename('CountryCode' = y)
#
#
#
#   # merge(GlobalMapping$SectorGroups) %>%
#   # merge(FuelTypes) %>%
#   # rename(SectorCode=SubsectorCode) %>%
#   # dplyr::select(SectorCode,FuelCode,el_inc,el_dem,el_cons,eff) %>%
#   # rename(FuelType=FuelCode)
#
#   return(Elasticities)
# }
#
#
#
#
#
#
#
#
# ################################################
# ## BUILDING A SINGLE OBJECT WITH ALL MATRICES ##
# ################################################
#
# # Note: currently, there are:
# #   - 50 Countries for which we have EC data but not prices (in dummy data)
# #   - 0 Countries for which we have prices but no EC data
# # TBC:
# #   - Update PricesInCPAT. The countries checked actually exist in the Prices_dom tab from CPAT.
# # Missing:
# #   - Prices for ecy: update table, apply 1 as a placeholder for the generation types
# #   - Missing prices for fuel.sectors, set 1 as placeholder.
#
#
# ObtainDataset   <- function(GlobalStartYear     = StartYear,
#                             GlobalEndYear       = EndYear,
#                             GlobalMapping       = Lookups,
#                             ListOfCountries     = FullCountryList,
#                             LoadExistent        = FALSE){
#
#   if(LoadExistent){
#
#     # Loading R.Data file already computed and stored in the directory
#     TempD           <- readRDS('ME_ProcessedDataD.rds')
#
#   }else{
#     # Recomputing and arranging together all needed data sets.
#
#     # Prices:
#     Prices          <- PreparePrices(InitYear = GlobalStartYear,
#                                      FinYear  = GlobalEndYear) %>%
#       mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#     # Energy Consumption data:
#     EC              <- PrepareInitEC(InitYear = GlobalStartYear,
#                                      FinYear  = GlobalEndYear) %>%
#       mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#     # Emission Factors
#     EF              <- PrepareEF(InitYear     = GlobalStartYear,
#                                  FinYear      = GlobalEndYear,
#                                  Mapping      = GlobalMapping)  %>%
#       mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#     # Elasticities
#     Elasticities    <- PrepareElasticities(Mapping         = GlobalMapping,
#                                            ListOfCountries = FullCountryList) %>%
#       mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#     # GDP relative to base year
#     GDPRelBase      <- CreateGDPRelativeToBase(InitYear     = GlobalStartYear,
#                                                FinYear      = GlobalEndYear)
#
#
#     AllCombinations <- expand.grid('CountryCode' = FullCountryList, 'SectorCode' = SectorCodeList, 'FuelType' = FuelList) %>%
#       mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#     TempD           <- AllCombinations %>%
#       left_join(EC) %>%
#       # Filling missing data with dummy values (ec = 0)
#       mutate_all(~replace(., is.na(.), 0)) %>%
#       left_join(Prices)  %>%
#       # Filling missing data with dummy values (rp = 1)
#       mutate_all(~replace(., is.na(.), 1)) %>%
#       left_join(GDPRelBase) %>%
#       # Filling missing data with dummy values (gdp = 1)
#       mutate_all(~replace(., is.na(.), 1)) %>%
#       left_join(Elasticities) %>%
#       # Filling missing data with dummy values (elasticities = 0)
#       mutate_all(~replace(., is.na(.), 0)) %>%
#       left_join(EF) %>%
#       # Filling missing data with dummy values (ef = 0.1)
#       mutate_all(~replace(., is.na(.), 0.1)) %>%
#       mutate('el_pr'   = el_dem + el_cons + el_dem*el_cons,
#              'cov_adj' = 1)
#
#     saveRDS(TempD, 'data/ME_ProcessedDataD.rds')
#   }
#
#   return(TempD)
# }
#
#
