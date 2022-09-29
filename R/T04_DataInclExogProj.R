#---------------------------------------------------------------------------------------
# Purpose:  Prepare data sets that combine historical information and external forecasts
#---------------------------------------------------------------------------------------


##########################
## GDP RELATIVE TO BASE ##
##########################

#  This function:
#   - Imports and formats data on GDP relative to base
#   - It uses data from WEO only. Needs to be complemented with WDI
# NOTE:
#   - This includes GDP projections beyond the first year of model calculations

#' GDP relative to base year
#'
#' @param DD Tibble with variables in matrix column format (D matrix)
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param RawGDPRelativeToBase
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'


PrepareGDPRelativeToBase <- function(DD,
                                     BaseL = BaseList,
                                     RawGDPRelativeToBase){

    # Base year for monetary values: To be read as a user-defined parameter
    # Projections of GDP growth can be recomputed based on this
    BaseY               <- BaseL$MonBaseYear
    LU                  <- BaseL$LU

    # Loading raw data that has 2018 as base year
    # This information does not cover the entire time span needed
    # RawGDPRelToBase     <- read_excel("data/Macro/GDPRelativeToBase.xlsx") %>%
    #   filter(Year %in% all_of(BaseL$AllYears))

    # NOTE: Dropping the dependency on Excel files
    RawGDPRelToBase     <- RawGDPRelativeToBase %>%
                            filter(Year %in% all_of(BaseL$AllYears))

    # Finding the GDPFactor for the chosen base year
    GDPFactBaseYear     <- RawGDPRelToBase %>%
                            filter(Year == BaseY) %>%
                            rename('BYGDPF' = GDPFactor) %>%
                            select(CountryCode, BYGDPF)

    # Re-scaling the GDP factor so that the base year = 1
    GDPRelToBase        <- RawGDPRelToBase %>%
                            left_join(GDPFactBaseYear, by = 'CountryCode') %>%
                            mutate(GDPFactor = GDPFactor/BYGDPF,
                                   Year = as.character(Year)) %>%
                            select(-BYGDPF) %>%
                            pivot_wider(names_from = 'Year', values_from = 'GDPFactor')


    # 2 steps for the adjustment:
    # - Computing the last available GDP growth rate
    # - Building a set of factors assuming that rate remains constant for the remaining years

    # This assumes there is info for all countries for that year, which is currently the case (for all countries in this dataset)
    LastYearAvailable   <- as.character(max(RawGDPRelToBase$Year))

    Step1.1             <- GDPRelToBase %>%
                            select(CountryCode, as.character(all_of(as.numeric(LastYearAvailable))-1), LastYearAvailable) %>%
                            set_names(c('CountryCode', 'Previous', 'Last')) %>%
                            # Gross GDP growth rate. Assumed constant in the remaining years
                            mutate(LastGDPgrowth = Last/Previous) %>%
                            select(CountryCode, LastGDPgrowth)

    # Expanding the rates found for all countries, sectors and fuels
    Step1.2             <- BaseL$IDcols %>%
                            left_join(Step1.1, by = 'CountryCode')


    # Computing the new GDP factors for all years after the
    # Filling a template matrix with information up to the last available year
    Step2               <- BaseL$TemplMat
    ColsToFill          <- which(colnames(Step2) %in% unique(RawGDPRelToBase$Year))

    Step2[,ColsToFill]  <- as.matrix(BaseL$IDcols %>%
                            left_join(GDPRelToBase, by = 'CountryCode') %>%
                            select(-c(CountryCode, SectorCode, FuelCode)))

    for(tt in c((as.numeric(LastYearAvailable)+1):BaseL$LastModYear)){ #tt<- 2034
          pos           <- which(colnames(Step2) == tt)
          Step2[, pos]  <- Step2[, pos-1]*Step1.2$LastGDPgrowth
    }

    # Building the data in matrix column format
    ExpGDPFactor        <- tibble(BaseL$IDcols,
                                  'GDPFactor' = Step2 )

    # Including the new data into the D matrix:
    DD                  <- DD %>%
                            left_join(ExpGDPFactor, by = c('CountryCode', 'SectorCode', 'FuelCode'))


    return(DD)

}

################################################
##   FUNCTION TO FILL DATA FOR MISSING YEARS  ##
################################################

# This function includes (as columns) the years that are missing in the data read

#' Include years missing in a dataframe
#'
#' @param TheDF Dataframe to modify
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param HistY Rule used to complete the data for historical missing years. (Current option: 'with_NA')
#' @param ProjY Rule used to complete the data for projected years (Current option: 'ConstantGrowth')
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'


CompleteYears           <- function(TheDF,                     # The data frame in which we will add the missing years
                                    BaseL = BaseList,          # List including relevant information about years chosen
                                    HistY = 'with_NA',         # Rule used to complete the data regarding historical years
                                    ProjY = 'ConstantGrowth'){ # Rule used to complete the data for projected years

    # Initializing the matrix that will be used for manipulation
    tempDF          <- TheDF

    #------------#
    #-- STEP 1 --#
    #------------#

    # Finding the years covered and the ones we will need to include

    # Years covered will be helpful when sorting back the columns of the table, while respecting the old order of some cols
    ExistingYears   <- colnames(BaseL$TemplMat)[(colnames(BaseL$TemplMat) %in% colnames(TheDF))]
    ColsToInclude   <- colnames(BaseL$TemplMat)[!(colnames(BaseL$TemplMat) %in% colnames(TheDF))]

    # The process is different for historical and projected years.
    # Historical years missing
    YBefore         <- ColsToInclude[ColsToInclude < BaseL$FirstModYear]


    #------------#
    #-- STEP 2 --#
    #------------#

    # Filling the missing values for the historical years:

    if(HistY == 'with_NA'){
        # Filling missing historical years with NAs
        for(yy in YBefore){
            tempDF      <- tempDF %>%
                            mutate('{yy}' := NA)
        }
    }

    # The result from this process will add historic years after the projected ones. This has to be sorted out.
    # This vector checks the years available to order AFTER historical years have been added
    # We only consider the available years included in the set of years of interest
    AvailableYears      <- colnames(BaseL$TemplMat)[(colnames(BaseL$TemplMat) %in% colnames(tempDF))]


    # Non-numerical columns in the ORIGINAL matrix (To keep the same order in the new one)
    NonNumericalCols    <- names(TheDF %>% select(!where(is.numeric)))

    # Sorting the columns so that the years are in order despite having added some historical years at the end in the previous step
    tempDF              <- tempDF %>%
                            # Ordering the columns having all non-numerical ones first, and all available years after
                            select(as.character(NonNumericalCols), as.character(AvailableYears))

    #------------#
    #-- STEP 3 --#
    #------------#

    # Including data on other missing years

    if(ProjY == 'ConstantGrowth'){
        # In general, this works for any missing year for which the previous 2 observations exist

        for(tt in c(ColsToInclude[!ColsToInclude %in% YBefore])){ # tt <- "2022"
            Tminus1     <- unlist( tempDF %>% select( as.character( as.numeric(tt)-1 ) ) )
            Tminus2     <- unlist( tempDF %>% select( as.character( as.numeric(tt)-2 ) ) )

            # Including the new column for each year
            tempDF      <- tempDF %>%
                            mutate('{tt}' := Tminus1*(Tminus1/Tminus2))
        }
    }


    # Returning the expanded data frame
    return(tempDF)

}



#######################################
##    INTERNATIONAL ENERGY PRICES    ##
#######################################

# Reads data on international prices of fossil fuels
# Some prices can come from different sources and/or markets
# Information by market is matched to each country. Information by source will be filtered by the user's choice
# The function returns a list. This list is then filtered according to the user choice.
# If the pre-processing is done live, the user-choice and the filtering can be added to the function.

#' Historical and projected international energy prices
#'
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'


PrepocessIntPricesList      <- function(BaseL,
                                        RInputs_InternationalPrices_RegionAssumptions,
                                        RInputs_InternationalPrices_RegionMarket,
                                        RInputs_InternationalPrices_IntPrices){



    # Loading the lookups:
    LU        <- BaseL$LU
    # Several steps needed:
    # Step1: Historical data for gso, die, lpg and ker, and expansion by country, sector and fuel
    #       - After data on regional prices for gso, die, lpg and ker is read, it has to be expanded by country instead of region
    #       - This is done for the full expansion Country, Sector, Fuel. Filter afterwards to keep only relevant fuels
    #       - Expanding data for all required years
    # Step2: Historical data and projection of prices for oop, coa and nga
    #       - Expanding information by country, sector, fuel, and for all years
    # Step3: Set the AdValFixed vector, at expanded level by country, sector and fuel, as a separate tibble
    # Step4: Report data as a list


    #------------#
    #-- STEP 1 --#
    #------------#

    # Prices of fuels to be assigned by region (This applies to: gso, die, lpg and ker)
    # IntPr_RegAssum  <- read_excel('data/Prices/RInputs_InternationalPrices.xlsx', sheet = 'RegionAssumptions') %>%
    #                     select(-FuelName) %>%
    #                     pivot_longer(-c('Region':'FuelType'), names_to = 'Year', values_to = 'Value') %>%
    #                     # Expanding the data from FuelType to FuelCode (instead of forcing a change of column name)
    #                     # This shouldn't change the dimensions of the current table, as it has no 'ele'
    #                     left_join(LU$FuelTypes, by = 'FuelType') %>%
    #                     select(Region, FuelCode, Year, Value) %>%
    #                     pivot_wider(names_from = 'Year', values_from = 'Value')

    # NOTE: Dropping the dependency on Excel files
    # load('data/RInputs_InternationalPrices_RegionAssumptions.rda')
    IntPr_RegAssum  <- RInputs_InternationalPrices_RegionAssumptions %>%
                        select(-FuelName) %>%
                        pivot_longer(-c('Region':'FuelType'), names_to = 'Year', values_to = 'Value') %>%
                        # Expanding the data from FuelType to FuelCode (instead of forcing a change of column name)
                        # This shouldn't change the dimensions of the current table, as it has no 'ele'
                        left_join(LU$FuelTypes, by = 'FuelType') %>%
                        select(Region, FuelCode, Year, Value) %>%
                        pivot_wider(names_from = 'Year', values_from = 'Value')


    # TempStep1: Historical data for gso, die, lpg and ker
    TempStep1       <- BaseL$IDcols %>%
                        left_join(LU$CountryCode, by = 'CountryCode') %>%
                        select(CountryCode, SectorCode, FuelCode, Region) %>%
                        left_join(IntPr_RegAssum, by = c('Region', 'FuelCode')) %>%
                        filter(FuelCode %in% c('gso', 'die', 'lpg', 'ker')) %>%
                        select(-Region)

    # Completing the dataframe time coverage:
    Step1           <- CompleteYears(TheDF = TempStep1)


    #------------#
    #-- STEP 2 --#
    #------------#

    # Information on which region applies for each country, and how to consider taxes (fixed or ad valorem)
    # IntPr_RegMarket   <- read_excel('data/Prices/RInputs_InternationalPrices.xlsx', sheet = 'RegionMarket') %>%
    #                       select(-Country) %>%
    #                       # Adopting PascalCase convention
    #                       rename('AdValFixed' = `Baseline taxes are ad-valorem or fixed?`,
    #                              'Market' = MarketAssumption)

    # NOTE: Dropping the dependency on Excel files
    # load('data/RInputs_InternationalPrices_RegionMarket.rda')
    IntPr_RegMarket   <- RInputs_InternationalPrices_RegionMarket %>%
                          select(-Country) %>%
                          # Adopting PascalCase convention
                          rename('AdValFixed' = `Baseline taxes are ad-valorem or fixed?`,
                                 'Market' = MarketAssumption)


    # Reading raw data and implementing first expansions
    # RawStep2            <- read_excel('data/Prices/RInputs_InternationalPrices.xlsx', sheet = 'IntPrices') %>%
    #                         # Dropping some metadata columns:
    #                         select(-c(SourceDescription, FuelDescription, Unit)) %>%
    #                         mutate(across(-c('Source':'Market'), as.numeric)) %>%
    #                         pivot_longer(-c('Source':'Market'), names_to = 'Year', values_to = 'Value') %>%
    #                         # Expanding to FuelCode is more robust than renaming the column
    #                         left_join(LU$FuelTypes, by = 'FuelType') %>%
    #                         select(Source, FuelCode, Market, Year, Value) %>%
    #                         filter(Year %in% BaseL$AllYears) %>%
    #                         pivot_wider(names_from = 'Year', values_from = 'Value') %>%
    #                         rename('MarketRaw' = Market) %>%
    #                         # Adding information on mappings to country, regions and markets
    #                         left_join(LU$ExpMarketIntPr, by = 'MarketRaw') %>%
    #                         left_join(IntPr_RegMarket, by = 'Market') %>%
    #                         right_join(BaseL$IDcols, by = c('CountryCode', 'FuelCode')) %>%
    #                         # AdValFixed could be recorded to a separate file if needed
    #                         select(Source, CountryCode, SectorCode, FuelCode, where(is.numeric))

    # NOTE: Dropping the dependency on Excel files
    # load('data/RInputs_InternationalPrices_IntPrices.rda')
    RawStep2            <- RInputs_InternationalPrices_IntPrices %>%
                            # Dropping some metadata columns:
                            select(-c(SourceDescription, FuelDescription, Unit)) %>%
                            mutate(across(-c('Source':'Market'), as.numeric)) %>%
                            pivot_longer(-c('Source':'Market'), names_to = 'Year', values_to = 'Value') %>%
                            # Expanding to FuelCode is more robust than renaming the column
                            left_join(LU$FuelTypes, by = 'FuelType') %>%
                            select(Source, FuelCode, Market, Year, Value) %>%
                            filter(Year %in% BaseL$AllYears) %>%
                            pivot_wider(names_from = 'Year', values_from = 'Value') %>%
                            rename('MarketRaw' = Market) %>%
                            # Adding information on mappings to country, regions and markets
                            left_join(LU$ExpMarketIntPr, by = 'MarketRaw') %>%
                            left_join(IntPr_RegMarket, by = 'Market') %>%
                            right_join(BaseL$IDcols, by = c('CountryCode', 'FuelCode')) %>%
                            # AdValFixed could be recorded to a separate file if needed
                            select(Source, CountryCode, SectorCode, FuelCode, where(is.numeric))



    # Fully expanding the data to country, sector and fuel
    TempStep2           <- BaseL$IDcols %>%
                            left_join(RawStep2, by = c('CountryCode', 'SectorCode', 'FuelCode') ) %>%
                            filter(FuelCode %in% c('oop', 'coa', 'nga'))


    # Completing the information for missing years
    Step2               <- CompleteYears(TheDF = TempStep2)


    #------------#
    #-- STEP 3 --#
    #------------#

    # AdValFixed tibble by Country, Sector and Fuel
    Step3               <- BaseL$IDcols %>%
                            left_join(IntPr_RegMarket, by = 'CountryCode') %>%
                            select(CountryCode, SectorCode, FuelCode, AdValFixed)


    #------------#
    #-- STEP 4 --#
    #------------#

    # Reporting all variables as a list for further consolidation after receiving the user's choices

    IntPrices                   <- list()
    IntPrices$IPSingleSource    <- Step1            # information for gso, die, lpg and ker (single source)
    IntPrices$IPMultSources     <- Step2            # information for oop, coa and nga (multiple sources)
    IntPrices$TibAdValFixed     <- Step3            # Tibble with the AdValFixed vector

    return(IntPrices)

}


#########################################################################
##    INTERNATIONAL ENERGY PRICES AFTER SELECTION OF SOURCE BY USER    ##
#########################################################################

# THIS IS AN INTERMEDIATE FUNCTION
# IT USES PRE-PROCESSED DATA AND USER-DEFINED INPUTS, AND PRODUCES PRE-PROCESSED DATA FOR THE ALGORITHMS TO RUN

# This functions uses the international prices LIST obtained when running PrepocessIntPricesList
# It also takes as input the source of information selected by the user
# It reads the existing D matrix, and adds the international price information to it
# It also includes the AdValFixed to the D matrix

#' International Prices' source filtering
#'
#' @param DD Scenario-specific tibble with historical information and required structure in matrix column format (D-matrix)
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param IPList List containing the international prices datasets by source
#' @param SelSource Selection of source for international prices data
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'



PrepareInternationalPrices      <- function(DD        = DL$Scenario1,
                                            BaseL     = BaseList,
                                            IPList    = PrepocessIntPricesList(BaseList,
                                                                               RInputs_InternationalPrices_RegionAssumptions,
                                                                               RInputs_InternationalPrices_RegionMarket,
                                                                               RInputs_InternationalPrices_IntPrices),
                                            SelSource = 'IMF-IEA'){

    #------------#
    #-- STEP 1 --#
    #------------#

    # Treating the international price data that for die, gso, lpg and ker
    # Reading and creating a "layer" of international prices for the data section that is not affected by the user's choice

    # These prices come from different markets/regions, but are mapped already to each country, sector and fuel

    # Expanding this to all possible country, fuel and sector, so that NA will appear.
    Step1       <- BaseL$IDcols %>%
                    left_join(IPList$IPSingleSource, by = c('CountryCode', 'SectorCode', 'FuelCode'))
                    # Testing if I can run with NA prior to
                    # NAs can replaced by 0s, to then add the layers on top of each other

    #------------#
    #-- STEP 2 --#
    #------------#

    # Treating the international price data that for coa, nga and oop
    # Reading and creating a "layer" of international prices for the data section that will be affected by the user's choice


    # If the user provides a non-useful input, we resort to 'IMF-IEA' by default
    if(!SelSource %in% unique(IPList$IPMultSources$Source)){
        SelSource   <- 'IMF-IEA'
    }

    # Filtering the information according to the source selected by the user
    FilStep2        <- IPList$IPMultSources %>%
                        filter(Source == SelSource) %>%
                        select(-Source)

    # Expanding this to all possible country, fuel and sector, so that NA will appear.
    Step2           <- BaseL$IDcols %>%
                        left_join(FilStep2, by = c('CountryCode', 'SectorCode', 'FuelCode'))

    #------------#
    #-- STEP 3 --#
    #------------#

    # Joining both layers from Steps 1 and 2
    # coalesce takes the inputs from the first element, unless they are NAs, in which case it takes it from the second element
    # International prices of non-fossil fuels will appear as NA
    TempStep3       <- coalesce(Step1, Step2)

    # Transforming this into D matrix format
    # This step may be redundant, as TempStep3 was ordered by IDcols, but it makes it more robust
    Step3           <- tibble(BaseL$IDcols,
                              'IntPrices'  = as.matrix(TempStep3 %>% select(-c('CountryCode', 'SectorCode', 'FuelCode'))))

    # Including this into the D matrix

    DD              <- DD %>%
                        left_join(Step3, by = c('CountryCode', 'SectorCode', 'FuelCode'))

    #------------#
    #-- STEP 4 --#
    #------------#

    # Including the AdValFixed data into the D matrix
    DD              <- DD %>%
                        left_join(IPList$TibAdValFixed, by = c('CountryCode', 'SectorCode', 'FuelCode'))


    # Among the helpers used for domestic price forecasting, one requires international prices data:
    t0              <- which(colnames(DD$cs) == BaseL$FirstModYear) - 1

    # The inputs to compute the helper often include multiple NA
    # The process applied here is more robust than simpler ones tested
    TempAltFalseH4  <- data.frame('FalseH4' =  DD$cs[,t0] - (DD$PCtrl_alpha0 -1)*(DD$IntPrices[,t0] - DD$IntPrices[,t0-1]) ) %>%
                        mutate('Zero' = 0,
                               # Setting NAs in tempH4 to 0:
                               'tempH4noNA' = coalesce(FalseH4, Zero)) %>%
                        select(-FalseH4)

    # Computing the minimum between the formula and 0
    # Recall that cs is the negative of the consumer subsidy in the IMF dataset
    AltFalseH4      <- apply( TempAltFalseH4,
                              1,
                              FUN = min,
                              na.rm = TRUE)

    DD$H4           <- if_else(DD$cs[,t0] == 0,
                               # If true:
                               0,
                               # If false:
                               AltFalseH4)


    return(DD)

}
