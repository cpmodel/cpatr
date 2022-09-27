# Updated data preparation script to produce D matrix for calibration


Lookups                 <- list()

Lookups$CountryCode     <- read_excel("data/Lookups/Lookups.xlsx", sheet = "CountryCode")
Lookups$EFCompleting    <- read_excel("data/Lookups/Lookups.xlsx", sheet = "EmissionFactorsCompleting")
Lookups$LargeFuelList   <- read_excel("data/Lookups/Lookups.xlsx", sheet = "FuelCodes")
Lookups$FuelTypes       <- read_excel("data/Lookups/Lookups.xlsx", sheet = "FuelTypes")
Lookups$PowFuelTypes    <- read_excel("data/Lookups/Lookups.xlsx", sheet = "PowerFuelTypes")
Lookups$ProjFuelTypes   <- read_excel("data/Lookups/Lookups.xlsx", sheet = "ProjectionsFuelTypes")
Lookups$SectorGroups    <- read_excel("data/Lookups/Lookups.xlsx", sheet = "FlowSectorGroupMapEC")

#######################################
##    OBSERVED ENERGY CONSUMPTION    ##
#######################################

# Function returns a list with two tables (matrix columns)
#   - observed energy consumption at sector CODE level
#   - observed energy consumption at sector GROUP level

PrepareObsEC    <- function(InitYear  = StartYear,
                            FinYear   = EndYear,
                            LU        = Lookups){

    AllYears    <- as.character(InitYear:FinYear)

    # NOTE: If reading from an Excel file, adapt the 'irn' column as 'dbl' and not 'chr'
    TempEC      <- read_csv("data/EnergyConsumption/EnergyConsumption_1990_to_2019.csv")  %>%
                    rename('CountryCode' = country_code,
                           'Year' = year,
                           'FuelCode' = FuelType) %>%
                    filter(Year %in% AllYears) %>%
                    mutate(Variable = 'enc') %>%
                    select(-ele, -neu, -wav) %>%
                    pivot_longer(names_to = "SectorCode", values_to = "Value", cols = pow:oen) %>%
                    left_join(LU$SectorGroups) %>%
                    filter(SectorGroup != 'all') %>%
                    select(CountryCode, SectorGroup, SectorCode, FuelCode, Year, Value) %>%
                    pivot_wider(names_from = Year, values_from = Value)



    # NOTE:
    # Returning two matrices, one at SectorCode level and one at SectorGroup level of detail

    EC.obs.list                 <- list()

    # SectorCode level:
    TempEC.id                   <- TempEC %>%
                                    select(-where(is.numeric), -SectorGroup)
    TempEC.matrix               <- as.matrix(TempEC %>%
                                    select(as.character(AllYears)))
    EC.obs.list$s               <- tibble(TempEC.id, 'ec.obs' = TempEC.matrix)

    # SectorGroup level:
    Temp.aggreg                 <- TempEC %>%
                                    group_by(CountryCode, SectorGroup, FuelCode) %>%
                                    summarise(across(where(is.numeric), list(sum = ~ sum(., na.rm = TRUE)))) %>%
                                    ungroup()

    colnames(Temp.aggreg)       <- c('CountryCode', 'SectorGroup', 'FuelCode', AllYears)

    TempEC.sg.id                <- Temp.aggreg %>%
                                    select(CountryCode, SectorGroup, FuelCode)

    TempEC.sg.matrix            <- as.matrix(Temp.aggreg %>%
                                    select(as.character(AllYears)))

    EC.obs.list$g               <- tibble(TempEC.sg.id, 'ec.obs' = TempEC.sg.matrix)

    return(EC.obs.list)
}



#########################################
##    HINDCASTED ENERGY CONSUMPTION    ##
#########################################

# Function returns a list with two tables (matrix columns)
#   - observed energy consumption at sector CODE level
#   - observed energy consumption at sector GROUP level

PrepareHindEC    <- function(InitYear = StartYear,
                            FinYear   = EndYear,
                            LU        = Lookups){

    AllYears    <- as.character(InitYear:FinYear)

    # NOTE: If reading from an Excel file, adapt the 'irn' column as 'dbl' and not 'chr'
    RawHindEC   <- read_excel('data/MTOutputData/1_Raw/MTO_HC1_WB7_Latest.xlsx')  %>%
                    # Dropping excess columns:
                    select(-starts_with('Dist')) %>%
                    # Ensuring all data years are taken as numbers and not characters
                    mutate(across(.cols = all_of(AllYears), .fns = as.numeric )) %>%
                    distinct_all() %>%
                    rename(SectorGroup = SectorGrouping,
                           SectorCode = Sector,
                           FuelCode = FuelType) %>%
                    select(CPATIndicator, RCode, Variable, SectorGroup, SectorCode, FuelCode, where(is.numeric)) %>%
                    filter(Variable == 'ener',
                           !is.na(SectorGroup),
                           !is.na(SectorCode),
                           !is.na(FuelCode),
                           SectorCode != 'all',
                           substr(CPATIndicator,1,20) == "Energy consumption, ") %>%
                    separate(RCode, into = c('CountryCode',NA,NA,NA)) %>%
                    mutate(CountryCode = toupper(CountryCode)) %>%
                    select(CountryCode, SectorGroup, SectorCode, FuelCode, where(is.numeric))


    # NOTE:
    # Returning two matrices, one at SectorCode level and one at SectorGroup level of detail

    EC.hind.list        <- list()

    # SectorCode level:
    IDcols              <- RawHindEC %>%
                                select(-SectorGroup, -as.character(AllYears))
    TempEC.matrix       <- as.matrix(RawHindEC %>%
                                select(as.character(AllYears)))
    EC.hind.list$s      <- tibble(IDcols, 'ec.hind' = TempEC.matrix)


    # SectorGroup level:
    Temp.aggreg                 <- RawHindEC %>%
                                    group_by(CountryCode, SectorGroup, FuelCode) %>%
                                    summarise(across(where(is.numeric), list(sum = ~ sum(., na.rm = TRUE)))) %>%
                                    ungroup()

    colnames(Temp.aggreg)       <- c('CountryCode', 'SectorGroup', 'FuelCode', AllYears)

    TempEC.sg.id                <- Temp.aggreg %>%
                                    select(CountryCode, SectorGroup, FuelCode)

    TempEC.sg.matrix            <- as.matrix(Temp.aggreg %>%
                                                 select(as.character(AllYears)))

    EC.hind.list$g              <- tibble(TempEC.sg.id, 'ec.hind' = TempEC.sg.matrix)



    return(EC.hind.list)
}



#############################
##    GDP DATA FROM WEO    ##
#############################

# Updated values computed using WEO data from October 2021.
# GDP in real terms and domestic currency was used to reconstruct the ratio

ComputeGDPtoBase        <- function(InitYear     = StartYear,
                                    FinYear      = EndYear,
                                    BaseY        = BaseYear,
                                    WEOFile      = 'WEOOct2021all.csv'){

    # Load WEO data
    WEORaw              <- read.csv(paste0('data/Macro/',WEOFile)) %>%
                            rename('CountryCode' = ISO,
                                   'VarCode' = WEO.Subject.Code) %>%
                            # Selecting the relevant indicators (GDP in constant prices)
                            filter(VarCode %in% c('NGDP_R',       # GDP constant prices (LCU Billions)
                                                  'NGDP',         # GDP current prices (LCU Billions)
                                                  'NGDPD',        # GDP current prices (USD Billions)
                                                  'NGDPRPC',      # GDP per capita, constant prices (LCU units)
                                                  'NGDPPC')) %>%  # GDP per capita, current prices (LCU units)
                            select(CountryCode, VarCode, starts_with('X')) %>%
                            pivot_longer(starts_with('X'), names_to = 'Year', values_to = 'Value') %>%
                            mutate(Value = as.numeric(gsub(",", "", Value)))

    # A sub table only with information of base year GDP per country
    # SYR and VEN have no value for the base year
    TempCol             <- WEORaw %>%
                            filter(Year == paste0('X',BaseY),
                                   VarCode == 'NGDP_R') %>%
                            mutate(RefYearValue = Value) %>%
                            select(CountryCode, RefYearValue)


    GDP.ratio           <- WEORaw %>%
                            filter(VarCode == 'NGDP_R') %>%
                            left_join(TempCol, by = 'CountryCode') %>%
                            mutate('Ratio' = Value/RefYearValue) %>%
                            mutate(VarCode = 'gdpreltobase') %>%
                            select(CountryCode, VarCode, Year, Ratio) %>%
                            rename(Value = Ratio)

    GDP.tib.all         <- WEORaw %>%
                            rbind(GDP.ratio) %>%
                            mutate(Year = as.character(gsub("X", "", Year))) %>%
                            filter(Year %in% as.character(InitYear:FinYear))

    GDP.tib.list        <- list()
    GDP.mat.list        <- list()
    GDP.id.list         <- list()
    GDP.list            <- list()

    for(ii in unique(GDP.tib.all$VarCode)){

        j                   <- which(unique(GDP.tib.all$VarCode) == ii)

        GDP.tib.list[[j]]   <- GDP.tib.all %>%
                                filter(VarCode == ii) %>%
                                pivot_wider(names_from = Year, values_from = Value)

        GDP.mat.list[[j]]   <- GDP.tib.list[[j]] %>%
                                select(as.character(c(InitYear:FinYear))) %>%
                                as.matrix()

        GDP.id.list[[j]]    <- GDP.tib.list[[j]] %>%
                                select(CountryCode)

        # Note: For naming the variable using a dynamic way, check:
        # https://stackoverflow.com/questions/68170333/is-it-possible-to-name-a-column-of-a-tibble-using-a-variable-containing-a-charac
        GDP.list[[j]]       <- tibble(GDP.id.list[[j]], '{unique(GDP.tib.all$VarCode)[j]}' := GDP.mat.list[[j]])
    }

    return(GDP.list)

}








# #####################################
# ##    PRE-TAX HISTORICAL PRICES    ##
# #####################################
#
# # Extracting the required set of information from the historical price dataset
# PrepareHistPrices   <- function(InitYear   = StartYear,
#                                 FinYear    = EndYear,
#                                 Mapping    = Lookups,
#                                 FileHistPr = 'Expand_prices_for_cpat_all_years_2022_04_15' ) {
#
#     # Reading historical prices in expanded version
#     HistDomPr       <- readRDS(paste0('../data/Prices/2_Processed/',FileHistPr))
#
#     # NOTE: Prices are already expanded in the input file
#     TempPr          <- HistDomPr %>%
#                         filter(Year %in% c(InitYear:FinYear),
#                                Indicator == 'rp') %>%
#                         select(-Indicator) %>%
#                         pivot_wider(names_from = Year, values_from = Value)
#
#     # Separating the information into criteria and values to create a data matrix
#     Prices.ID       <- TempPr %>%
#                         select(CountryCode, FuelType, SectorCode)
#
#     Prices.Val.Mat  <- TempPr %>%
#                         select(as.character(InitYear:FinYear)) %>%
#                         as.matrix()
#
#     # Recording values in the desired output format
#     Prices          <- tibble(Prices.ID, rp.obs = Prices.Val.Mat)
#
#     # TBC: Add prices for wnd sol hyd ore nuc = 1 as placeholder
#
#     return(Prices)
# }
#
#
#
# ##############################
# ##     EMISSION FACTORS     ##
# ##############################
#
# #  This function:
# #   - Imports Emission Factors data
# #   - Expands the EF information from SectorGroup to SectorCode
# #   - Fills missing values according to rule read from the respective lookup table
# #   - Completes information by adding fuels with 0 EFs
#
# PrepareEF           <- function(InitYear  = StartYear,
#                                 FinYear   = EndYear,
#                                 Mapping   = Lookups) {
#
#     # Five steps to build EF table:
#     # 1. T1: Expanding existing Sector Groups to Sectors in raw data
#     # 2. T2: Dealing with SectorGroup 'all' (which refers to 'all other non specified elsewhere')
#     # 3. T3: Filling missing values using EF data from other sector.fuel, according to rules specified in lookup table
#     # 4. T4: Including all fuels with no emissions (excluded from EF raw table)
#     # 5. T5: Appending all relevant subtables
#
#     # STEP 1: ------------------------------------------------------------------
#     # Expanding existing groups:
#     #       a) T0: Raw Imported Data for further use
#     #       b) T1: Filter out 'all' from T0.
#     #       c) T1: Join with lookup to expand SectorGroup into SectorCode.
#     #       d) T1: Add back the rod & avi
#     #       e) T1: Create ID1 variable 'Country.Sector.Fuel'
#
#     # Subset of map from Groups to sectors, excluding 'all' and the unused FlowCode
#     EFSectorGroups  <- Mapping$SectorGroups %>%
#                         filter(SectorGroup != 'all') %>%
#                         select(-FlowCode)
#
#     # Step 1, item a)
#     # Importing raw table. (includes 'rod' and 'avi' among SectorGroups -> it already expands 'tra' into some SectorCode)
#     T0              <- read_excel("../data/EmissionFactors/EmissionFactorsCountryFuel.xlsx", sheet = "EmissionFactorsCountry") %>%
#                         filter(Notes == "ef CO2") %>%   # Not needed given new form of input. Left in case data is unfiltered.
#                         select(CountryCode, SectorCode, FuelType, ef) %>%
#                         rename(SectorGroup = SectorCode)
#
#     # T0 contains some sectors (e.g. 'avi' and 'rod') under the column SectorGroups.
#     # This will be excluded when performing the inner_join in T1, so need to be included after it.
#     SectorsToAdd    <- unique(T0$SectorGroup) %>%
#                         subset(!. %in% c('all',(intersect(unique(T0$SectorGroup), unique(EFSectorGroups$SectorGroup)))))
#
#
#     # Table to append
#     T0.add          <- T0[T0$SectorGroup %in% SectorsToAdd,] %>%
#                         rename(SectorCode = SectorGroup)
#
#
#     # Step 1, items b), c), d) and e)
#     # Table resulting from Step 1:
#     T1              <- T0 %>%
#                         # Expanding Sector Groups into Sectors
#                         # This join already excludes 'all', but also 'rod' and 'avi' from original table.
#                         inner_join(EFSectorGroups, by = "SectorGroup") %>%
#                         #rename(SectorCode = Sector) %>%
#                         select(CountryCode, SectorCode, FuelType, ef) %>%
#                         # Adding excluded sectors originally mislabled as SectorGroups (except 'all') from original table
#                         rbind(T0.add) %>%
#                         mutate('ID1' = paste(CountryCode,SectorCode,FuelType, sep = '.'))
#
#
#     # STEP 2: ------------------------------------------------------------------
#     # Dealing with 'all':
#     #       a) T2.1:        Keep only Sector = 'all' from original table.
#     #       b) T2.2:        Combine all expected countries, sectors and (non-zero EFs) fuel types.
#     #       c) T2.2:        Create ID1 as in T1. Drop all rows that exist in T1.
#     #       d) T2.1 & T2.2: Create ID2 column for 'country.fuel' (not sector)
#     #       e) T2:          Join T2.1 and T2.2 by ID2. (Expand T2.1 to the required number of rows given by 'country.fuel' in T2.2)
#     #       f) T2:          Drop ID2
#
#
#     # Step 2, items a) and d)
#     # Table with VALUES to be expanded to 'all' other sectors
#     T2.1            <- T0 %>%
#                         filter(SectorGroup == 'all') %>%
#                         mutate('ID2' = paste(CountryCode,FuelType, sep = '.')) %>%
#                         select(ID2, ef)
#
#
#     # List of countries covered in EF database
#     CountriesEF     <- unique(T0$CountryCode)
#
#     # Vector of fuels with non-zero emission factors
#     NonZeroFuels    <- unique(T0$FuelType) %>%
#                         intersect(unique(Mapping$EFCompleting$Fuel))
#
#     # Excluding 'neu' and 'wav' from all sectors (TBC: Desired?)
#     SectorEF        <- unique(Mapping$SectorGroups$SectorCode) %>%
#                         subset(!. %in% c('neu','wav'))
#
#     # Step 2, items b), c) and d)
#     # Table with list of 'all other sectors' over which to expand the values contained in T2.1
#     T2.2            <- expand.grid('CountryCode' = CountriesEF, 'SectorCode' = SectorEF, 'FuelType' = NonZeroFuels) %>%
#                         mutate('ID1' = paste(CountryCode, SectorCode, FuelType, sep = '.'),
#                                'ID2' = paste(CountryCode, FuelType, sep = '.')) %>%
#                         filter(! ID1 %in% T1$ID1 )
#
#     # Step 2, item e) and f)
#     # Table resulting from Step 2 (Expansion of 'all other sectors')
#     T2              <- tibble(left_join(T2.2, T2.1, by = 'ID2')) %>%
#                         select(- ID2) %>%
#                         mutate('ID3' = paste(SectorCode,FuelType, sep = '.'))
#
#     # STEP 3: ------------------------------------------------------------------
#     # Filling missing values of EF for the 'sector.fuel' according to rules defined in lookup table.
#     #       a) T3.1:    Create table with rules on how to fill the missing data (at SectorGroup level --> Use T0 to fill it)
#     #       b) T3.2:    Expand the table version of sectors to consider (from T3.1), include countries and source to fill value
#     #       c) T3.3:    Use T0 (its at SectorGroup level), to create a table from where the values will be taken with an ID to match
#     #       d) T3:      Final table filled with EF data from other sector.fuel according to rules in lookup table
#
#
#     # Step 3, item a)
#     # Create table with rules on how to fill the missing data (at SectorGroup level --> Use T0 to fill it)
#     T3.1        <- Mapping$EFCompleting %>%
#                     filter(!is.na(CompleteFrom)) %>%
#                     mutate('ID3' = paste(SectorGroup,Fuel, sep = '.')) %>%
#                     select(-SourceSectorGroupFuel) %>%
#                     rename(SectorCode = SectorGroup)  # No groups contained in mapping rules
#
#         # Step 3, item b)
#     # Expand the table version of sectors to consider (from T3.1), include countries and source to fill value
#     T3.2        <- expand.grid('CountryCode' = unique(CountriesEF),
#                                'ID3'         = unique(T3.1$ID3)) %>%
#                     # Adding the ID at Sector level
#                     mutate('ID1' = paste(CountryCode,ID3, sep = '.')) %>%
#                     # Including the matching rules
#                     left_join(T3.1, by = 'ID3') %>%
#                     mutate('IDSource' = paste(CountryCode, CompleteFrom, sep = '.')) %>%
#                     select(ID1, IDSource)
#
#
#     # Step 3, item c)
#     # Use T0 (its at SectorGroup level), to create a table from where the values will be taken with an ID to match
#     T3.3        <- T0 %>%
#                     mutate('IDSource' = paste(CountryCode, SectorGroup, FuelType, sep = '.')) %>%
#                     select(IDSource, ef)
#
#
#     # Step 3, item d)
#     # Final table filled with EF data from other sector.fuel according to rules in lookup table
#     T3          <- T3.2 %>%
#                     left_join(T3.3, by = 'IDSource') %>%
#                     select(- IDSource) %>%
#                     separate(ID1, into = c('CountryCode', 'SectorCode', 'FuelType')  )
#
#
#     # STEP 4: ------------------------------------------------------------------
#     # Including other fuels with 0 EF:
#     #       a) T4: Creating a table with combination of Countries, all other fuels and all sectors
#
#     # List of all (14) fuels
#     FuelList    <- unique(Mapping$FuelTypes$FuelCode)
#
#     # List of fuels assigned a 0 EF
#     ZeroFuels   <- FuelList %>%
#                     subset(!. %in% NonZeroFuels)
#
#     # Step 4, item a)
#     # Creating a table with combination of Countries, all other fuels and all sectors
#     T4          <- expand.grid('CountryCode' = unique(CountriesEF),
#                                'SectorCode'  = unique(SectorEF),
#                                'FuelType'    = unique(ZeroFuels)) %>%
#                     mutate('ef' = 0)
#
#
#     # STEP 5: ------------------------------------------------------------------
#     # Append and format the resulting table:
#     #       a) Drop auxiliary columns from all sub-tables
#     #       b) EF: append all subtables and arrange them
#     # NOTE: the expanding of 'all' sectors, and the completing of missing elements with particular rules
#     #       creates double entries for several country.sector.fuel cases.
#     #       This can be seen when including T3 on the binding routine
#
#
#     # Step 5, item a)
#     # T1: Expanding existing Sector Groups to Sectors in raw data
#     T1f         <- T1 %>%
#                     select(CountryCode, SectorCode, FuelType, ef) %>%
#                     mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.')) %>%
#                     filter(!is.na(ef))
#
#
#     # Note the existence of missing values despite the filling in
#     # This fill should take place before 'all' sectors non specified elsewhere
#     # The order was changed when checking for duplicated values
#     # T3: Filling missing values using EF data from other sector.fuel, according to rules specified in lookup table
#     T3f         <- T3 %>%
#                     select(CountryCode, SectorCode, FuelType, ef) %>%
#                     mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.')) %>%
#                     filter(!is.na(ef),
#                            !ID %in% T1f$ID)
#
#
#     # T2: Dealing with SectorGroup 'all' (which refers to 'all other non specified elsewhere')
#     # The order was changed when checking for duplicated values
#     T2f         <- T2 %>%
#                     select(CountryCode, SectorCode, FuelType, ef) %>%
#                     mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.')) %>%
#                     filter(!is.na(ef),
#                            !ID %in% T1f$ID,
#                            !ID %in% T3f$ID)
#
#
#     # T4: Including all fuels with no emissions (excluded from EF raw table)
#     T4f         <- T4 %>%
#                     select(CountryCode, SectorCode, FuelType, ef) %>%
#                     mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#
#     # Step 5, item b)
#     EF          <- rbind(T1f, T2f, T3f, T4f) %>%
#                     arrange(ID)
#
#
#     # Check for duplicates:
#     # EF %>%
#     #     #select(-ef) %>%
#     #     group_by(ID) %>%
#     #     arrange(ID) %>%
#     #     filter(n()>1)
#
#     return(EF)
# }
#
#
#
# ##########################
# ##     ELASTICITIES     ##
# ##########################
#
# #  This function:
# #   - Imports data on Elasticities
# #   - Expands data (replicates) for all countries
#
# PrepareElasticities     <- function(Mapping         = Lookups,
#                                     ListOfCountries = FullCountryList) {
#
#     Elasticities        <- read_excel("../data/Elasticities/Elasticities.xlsx", sheet = "Old") %>%
#                             rename(SectorGroup = Sector) %>%
#                             filter(FuelType %in% FuelList) %>%
#                             inner_join(Mapping$SectorGroup, by = 'SectorGroup') %>%
#                             select(SectorCode, FuelType, el_inc, el_dem, el_cons, eff) %>%
#                             merge(ListOfCountries) %>%
#                             rename('CountryCode' = y)
#
#
#     return(Elasticities)
# }







################################################
## BUILDING A SINGLE OBJECT WITH ALL MATRICES ##
################################################

# Note: currently, there are:
#   - 50 Countries for which we have EC data but not prices (in dummy data)
#   - 0 Countries for which we have prices but no EC data
# TBC:
#   - Update PricesInCPAT. The countries checked actually exist in the Prices_dom tab from CPAT.
# Missing:
#   - Prices for ecy: update table, apply 1 as a placeholder for the generation types
#   - Missing prices for fuel.sectors, set 1 as placeholder.


# ObtainDataset   <- function(GlobalStartYear     = StartYear,
#                             GlobalEndYear       = EndYear,
#                             GlobalMapping       = Lookups,
#                             ListOfCountries     = FullCountryList,
#                             LoadExistent        = FALSE){
#
#     if(LoadExistent){
#
#         # Loading R.Data file already computed and stored in the directory
#         TempD           <- readRDS('../data/ME_ProcessedDataD.rds')
#
#     }else{
#         # Recomputing and arranging together all needed data sets.
#
#         # Prices:
#         Prices          <- PrepareHistPrices(InitYear = GlobalStartYear,
#                                              FinYear  = GlobalEndYear) %>%
#                             mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#         # Energy Consumption data:
#         # Observed
#         EC.obs          <- PrepareHistEC(InitYear = GlobalStartYear,
#                                          FinYear  = GlobalEndYear) %>%
#                             mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#         # Initial data where projections will be stored
#         EC.id           <- EC.obs %>%
#                             select(CountryCode:SectorCode)
#
#         ec              <- EC.obs$ec.obs[,as.character(GlobalStartYear)] %>%
#                             cbind(EC.obs$ec.obs[,-1]*0)
#
#         colnames(ec)    = as.character(GlobalStartYear:GlobalEndYear)
#
#         EC              <- tibble(EC.id, ec) %>%
#                             mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#         # Emission Factors
#         EF              <- PrepareEF(InitYear     = GlobalStartYear,
#                                      FinYear      = GlobalEndYear,
#                                      Mapping      = GlobalMapping)  %>%
#                             mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#
#         # Elasticities
#         Elasticities    <- PrepareElasticities(Mapping         = GlobalMapping,
#                                                ListOfCountries = FullCountryList) %>%
#                             mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#         # GDP relative to base year
#         GDPlist         <- ComputeGDPtoBase(InitYear     = GlobalStartYear,
#                                             FinYear      = GlobalEndYear)
#
#
#         AllCombinations <- expand.grid('CountryCode' = unique(FullCountryList),
#                                        'SectorCode'  = unique(SectorCodeList),
#                                        'FuelType'    = unique(FuelList))%>%
#                             mutate(ID = paste(CountryCode, SectorCode, FuelType, sep = '.'))
#
#
#         TempD           <- AllCombinations %>%
#                             left_join(EC) %>%
#                             left_join(EC.obs) %>%
#                             # Filling missing data with dummy values (ec = 0)
#                             mutate_all(~replace(., is.na(.), 0)) %>%
#                             left_join(Prices)  %>%
#                             # Filling missing data with dummy values (rp = 1)
#                             mutate_all(~replace(., is.na(.), 1)) %>%
#                             left_join(Elasticities) %>%
#                             # Filling missing data with dummy values (elasticities = 0)
#                             mutate_all(~replace(., is.na(.), 0)) %>%
#                             left_join(EF) %>%
#                             # Filling missing data with dummy values (ef = 0.1)
#                             mutate_all(~replace(., is.na(.), 0.1)) %>%
#                             mutate('el_pr'   = el_dem + el_cons + el_dem*el_cons,
#                                    'cov_adj' = 1)
#
#         # Adding the elements in the GDP list
#         for(j in c(1:length(GDPlist))){
#
#             TempD       <- TempD %>%
#                             left_join(GDPlist[[j]])
#                             # Filling missing data with dummy values (gdp = 1)
#                             # mutate_all(~replace(., is.na(.), 1))
#         }
#
#         # Check: List of double entries
#         # TempD %>% select(CountryCode, ID) %>% group_by(ID) %>% arrange(ID) %>%
#         #     filter(n()>1)
#
#         saveRDS(TempD, '../data/ME_ProcessedDataD2.rds')
#     }
#
#     return(TempD)
# }



#TestD   <- ObtainDataset()

