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

#' Computes GDP relative to a user-defined base year
#'
#' @param DD Tibble with variables in matrix column format (D matrix)
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param RawGDPRelativeToBase
#'
#' @return The DD tibble provided as input, where GDP has been adjusted to reflect the choice of the base year by the user.
#' @export
#' @import tidyr dplyr purrr
#'


PrepareGDPRelativeToBase <- function(DD,
                                     BaseL,
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
                            dplyr::filter(Year %in% all_of(BaseL$AllYears))

    # Finding the GDPFactor for the chosen base year
    GDPFactBaseYear     <- RawGDPRelToBase %>%
                            dplyr::filter(Year == BaseY) %>%
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
                            select(CountryCode,
                                   as.character(all_of(as.numeric(LastYearAvailable))-1),
                                   all_of(LastYearAvailable)) %>%
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

#' Fills in the missing trailing or tail data of a dataframe when the model runs for more years than the data is available.
#'
#' @param TheDF Dataframe to modify
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param HistY Rule used to complete the data for historical missing years. (Current option: 'with_NA')
#' @param ProjY Rule used to complete the data for projected years (Current option: 'ConstantGrowth')
#'
#' @return The inputed Dataframe after filling values on years for which there was no data.
#' @export
#' @import tidyr dplyr purrr
#'


CompleteYears           <- function(TheDF,                     # The data frame in which we will add the missing years
                                    LocalBaseL,
                                    HistY = 'with_NA',         # Rule used to complete the data regarding historical years
                                    ProjY = 'ConstantGrowth',  # Rule for projected years: ConstantGrowth, ConstantValue, Relative
                                    ReferenceVal){             # Time series to be used as reference if ProjY == 'Relative'

    # Initializing the matrix that will be used for manipulation
    tempDF          <- TheDF

    #------------#
    #-- STEP 1 --#
    #------------#

    # Finding the years covered and the ones we will need to include

    # Years covered will be helpful when sorting back the columns of the table, while respecting the old order of some cols
    ExistingYears   <- colnames(LocalBaseL$TemplMat)[(colnames(LocalBaseL$TemplMat) %in% colnames(TheDF))]
    ColsToInclude   <- colnames(LocalBaseL$TemplMat)[!(colnames(LocalBaseL$TemplMat) %in% colnames(TheDF))]

    # The process is different for historical and projected years.
    # Historical years missing
    YBefore         <- ColsToInclude[ColsToInclude < LocalBaseL$FirstModYear]


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
    AvailableYears      <- colnames(LocalBaseL$TemplMat)[(colnames(LocalBaseL$TemplMat) %in% colnames(tempDF))]


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
    }else if(ProjY == 'ConstantValue'){

        # Retrieving the last year available to fill the remaining years

        for(tt in c(ColsToInclude[!ColsToInclude %in% YBefore])){
            tempDF      <- tempDF %>%
                              mutate('{tt}' := unlist( tempDF %>% select( as.character( max(ExistingYears) ) ) ) )
        }
    }else if(ProjY == 'Relative'){

        # Evolution follows the growth rate of another variable

        for(tt in c(ColsToInclude[!ColsToInclude %in% YBefore])){

            # Reference time series values
            Ref_T       <- as.numeric(ReferenceVal[as.character( as.numeric(tt) )])
            Ref_Tminus1 <- as.numeric(ReferenceVal[as.character( as.numeric(tt) - 1 )])

            # Previous observation on the current series
            Tminus1     <- unlist( tempDF %>% select( as.character( as.numeric(tt)-1 ) ) )

            # Including the new column for each year
            tempDF      <- tempDF %>%
                            mutate('{tt}' := Tminus1*(Ref_T/Ref_Tminus1))
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


PreprocessIntPricesList     <- function(BaseL,
                                        RInputs_InternationalPrices_RegionAssumptions,
                                        RInputs_InternationalPrices_RegionMarket,
                                        RInputs_InternationalPrices_IntPrices){



    # Loading the lookups:
    LU        <- BaseL$LU
    # Several steps needed:
    # Step1: Historical data and projection of prices for oop, coa and nga
    #       - Expanding information by country, sector, fuel, and for all years
    # Step2: Historical data for gso, die, lpg and ker, and expansion by country, sector and fuel
    #       - After data on regional prices for gso, die, lpg and ker is read, it is expanded by country instead of region
    #       - This is done for the full expansion Country, Sector, Fuel. Filter afterwards to keep only relevant fuels
    #       - Expanding data for all required years
    # Step3: Set the AdValFixed vector, at expanded level by country, sector and fuel, as a separate tibble
    # Step4: Report data as a list


    #------------#
    #-- STEP 1 --#
    #------------#

    # Information on which region applies for each country, and how to consider taxes (fixed or ad valorem)
    # IntPr_RegMarket   <- read_excel('data/Prices/RInputs_InternationalPrices.xlsx', sheet = 'RegionMarket') %>%
    #                       select(-Country) %>%
    #                       # Adopting PascalCase convention
    #                       rename('AdValFixed' = `Baseline taxes are ad-valorem or fixed?`,
    #                              'Market' = MarketAssumption)

    IntPr_RegMarket   <- RInputs_InternationalPrices_RegionMarket %>%
                          select(-Country) %>%
                          # Adopting PascalCase convention
                          rename('AdValFixed' = `Baseline taxes are ad-valorem or fixed?`,
                                 'Market' = MarketAssumption)

    # Matching the first set of data (multiple sources) by region
    # This step still provides information in nominal terms
    RawStep1            <- RInputs_InternationalPrices_IntPrices %>%
                            # Dropping some metadata columns:
                            select(-c(SourceDescription, FuelDescription, Unit)) %>%
                            mutate(across(-c('Source':'Market'), as.numeric)) %>%
                            # To avoid receiving the message: "NAs introduced by coercion"
                            suppressWarnings() %>%
                            pivot_longer(-c('Source':'Market'), names_to = 'Year', values_to = 'Value') %>%
                            # Expanding to FuelCode is more robust than renaming the column
                            left_join(LU$FuelTypes, by = 'FuelType') %>%
                            select(Source, FuelCode, Market, Year, Value) %>%
                            dplyr::filter(Year %in% BaseL$AllYears) %>%
                            pivot_wider(names_from = 'Year', values_from = 'Value') %>%
                            rename('MarketRaw' = Market) %>%
                            # Adding information on mappings to country, regions and markets
                            left_join(LU$ExpMarketIntPr, by = 'MarketRaw') %>%
                            left_join(IntPr_RegMarket, by = 'Market') %>%
                            right_join(BaseL$IDcols, by = c('CountryCode', 'FuelCode')) %>%
                            # AdValFixed could be recorded to a separate file if needed
                            select(Source, CountryCode, SectorCode, FuelCode, where(is.numeric))


    # Fully expanding the data to country, sector and fuel for oop, coa and nga
    # Still in nominal terms
    TempStep1.1         <- BaseL$IDcols %>%
                            left_join(RawStep1, by = c('CountryCode', 'SectorCode', 'FuelCode') ) %>%
                            dplyr::filter(FuelCode %in% c('oop', 'coa', 'nga'))


    # TempStep1.2: Historical data for oop, coa and nga converted into real terms
    # NOTE: Only THESE international prices are transformed into real terms using the GDP deflator instead of the CPI
    TempStep1.2         <- TempStep1.1 %>%
                            pivot_longer(cols = -c(CountryCode, SectorCode, FuelCode, Source),
                                         names_to = 'Year', values_to = "Value") %>%
                            left_join(BaseL$DiscountFactorDefl, by = "Year") %>%
                            rename("CurrentValue" = Value) %>%
                            mutate(Value = CurrentValue * DiscountFactor) %>%
                            select(-c(CurrentValue, DiscountFactor)) %>%
                            pivot_wider(names_from = 'Year', values_from = 'Value')

    # International prices of oil, coal and natural gas:
    # Completing the information for missing years.
    # We have data for the entire period, so the process should be redundant
    Step1               <- CompleteYears(TheDF = TempStep1.2,
                                         LocalBaseL = BaseL,
                                         ProjY = 'ConstantValue')

    #------------#
    #-- STEP 2 --#
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


    # TempStep2: Historical data for gso, die, lpg and ker
    # In nominal terms
    TempStep2       <- BaseL$IDcols %>%
                        left_join(LU$CountryCode, by = 'CountryCode') %>%
                        select(CountryCode, SectorCode, FuelCode, Region) %>%
                        left_join(IntPr_RegAssum, by = c('Region', 'FuelCode')) %>%
                        dplyr::filter(FuelCode %in% c('gso', 'die', 'lpg', 'ker')) %>%
                        select(-Region)

    # Step2: Historical data for gso, die, lpg and ker converted into real terms
    # NOTE:   The completion of this data set depends on the price source selected by the user
    #         This, as the evolution of these prices depends on the evolution of oil prices
    Step2           <- TempStep2 %>%
                        pivot_longer(cols = -c(CountryCode, SectorCode, FuelCode),
                                     names_to = 'Year', values_to = "Value") %>%
                        mutate(Year = as.numeric(Year)) %>%
                        left_join(BaseL$DiscountFactorCPI, by = "Year") %>%
                        rename("CurrentValue" = Value) %>%
                        mutate(Value = CurrentValue * DiscountFactor) %>%
                        select(-c(CurrentValue, DiscountFactor)) %>%
                        pivot_wider(names_from = 'Year', values_from = 'Value')


    #------------#
    #-- STEP 3 --#
    #------------#

    # AdValFixed tibble by Country, Sector and Fuel
    # NOTE: This extends the market category for all fuels, despite not having an international price for some of them
    Step3               <- BaseL$IDcols %>%
                            left_join(IntPr_RegMarket, by = 'CountryCode') %>%
                            select(CountryCode, SectorCode, FuelCode, AdValFixed)


    #------------#
    #-- STEP 4 --#
    #------------#

    # Reporting all variables as a list for further consolidation after receiving the user's choices
    # Prices from single and multiple sources have all been converted into real terms

    IntPrices                   <- list()
    IntPrices$IPMultSources     <- Step1            # information for oop, coa and nga (multiple sources)
    IntPrices$IPSingleSource    <- Step2            # information for gso, die, lpg and ker (single source)
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


PrepareInternationalPrices      <- function(DD,
                                            BaseL,
                                            IPList,
                                            SelSource){


    # IPList contains international prices from single/multiple sources, in real terms
    # It is computed on the main function, after receiving the input from the user on the selected source

    #------------#
    #-- STEP 1 --#
    #------------#

    # This fuels are done first, as they depend on the user's choice of source, and they affect the evolution of other fuels
    # Treating the international price data that for coa, nga and oop
    # Reading and creating a "layer" of international prices for the data section that will be affected by the user's choice


    # If the user provides a non-useful input, we resort to 'IMF-IEA' by default
    if(!SelSource %in% unique(IPList$IPMultSources$Source)){
        SelSource   <- 'IMF-WB'
    }

    # Filtering the information according to the source selected by the user
    FilStep1        <- IPList$IPMultSources %>%
                        dplyr::filter(Source == SelSource) %>%
                        select(-Source)

    # Expanding this to all possible combinations of country, fuel and sector, so that NA will appear.
    Step1           <- BaseL$IDcols %>%
                        left_join(FilStep1, by = c('CountryCode', 'SectorCode', 'FuelCode'))


    #------------#
    #-- STEP 2 --#
    #------------#

    # Treating the international price data that for die, gso, lpg and ker
    # Reading and creating a "layer" of international prices for the data section that is not affected by the user's choice

    # These prices come from different markets/regions, but are mapped already to each country, sector and fuel
    # However, projections depend on oil prices, which come from multiple sources

    # Vector with selected oil prices (in real USD per bbl)
    OilPrices     <- Step1 %>%
                      dplyr::filter(FuelCode == 'oop') %>%
                      select(-c(CountryCode, SectorCode, FuelCode)) %>%
                      distinct()

    # Completing the time series for all years:
    TempStep2     <- CompleteYears(TheDF = IPList$IPSingleSource,
                                   LocalBaseL = BaseL,
                                   ProjY = 'Relative',
                                   ReferenceVal = OilPrices)

    # Expanding this to all possible country, fuel and sector, so that NA will appear.
    Step2         <- BaseL$IDcols %>%
                      left_join(TempStep2, by = c('CountryCode', 'SectorCode', 'FuelCode'))


    #------------#
    #-- STEP 3 --#
    #------------#

    # Joining both layers from Steps 1 and 2
    # coalesce takes the inputs from the first element, unless they are NAs, in which case it takes it from the second element
    # International prices of non-fossil fuels will appear as NA
    TempStep3.1     <- coalesce(Step1, Step2)

    # These prices are all expressed in multiple units. Some of those need to be converted into USD/GJ
    # Conversion factors aftect the energy unit, which is on the denominator of the units, hence the division
    TempStep3.2     <- TempStep3.1 %>%
                        pivot_longer(cols = -c(CountryCode, SectorCode, FuelCode), names_to = 'Year', values_to = 'Value') %>%
                        pivot_wider(names_from = 'FuelCode', values_from = 'Value') %>%
                        mutate(coa = coa / as.numeric(BaseL$LU$ConvFact %>%
                                                        dplyr::filter(FuelCode == 'coa') %>%
                                                        select(ConversionFactor) ),
                               nga = nga / as.numeric(BaseL$LU$ConvFact %>%
                                                        dplyr::filter(FuelCode == 'nga') %>%
                                                        select(ConversionFactor) ),
                               oop = oop / as.numeric(BaseL$LU$ConvFact %>%
                                                        dplyr::filter(FuelCode == 'oop') %>%
                                                        select(ConversionFactor) )) %>%
                        pivot_longer(cols = -c(CountryCode, SectorCode, Year), names_to = 'FuelCode', values_to = 'Value') %>%
                        pivot_wider(names_from = 'Year', values_from = 'Value')

    # Transforming this into D matrix format
    # At this stage, prices are already in real terms per GJ (or liter, for the respective fuels)
    # TempStep3 was not ordered by IDcols, so Step 3 has to be built as this:
    Step3           <- tibble((TempStep3.2 %>% select(CountryCode, SectorCode, FuelCode)),
                              'IntPrices'  = as.matrix(TempStep3.2 %>% select(-c('CountryCode', 'SectorCode', 'FuelCode'))))

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
