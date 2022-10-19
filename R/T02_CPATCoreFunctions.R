#---------------------------------------------------------------------------------------
# Purpose:  Lay down the core functions for the mitigation module
#---------------------------------------------------------------------------------------


#####################
## Price Algorithm ##
#####################

#  This function:
#   - Computes the new domestic fossil fuel prices and its components for a given time step
#   - Reads the D matrix and fills the prices and price components matrices for the selected year


# Function to forecast domestic prices based on historical data, CPAT assumptions, and user-defined options

#' Domestic prices forecasting
#'
#' @param DD Tibble with historical information and required structure in matrix column format (D-matrix)
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param PolInputs List of policy details to consider for the current scenario
#' @param ScenName Scenario name, consistent with the names used in PolInputs
#' @param Year Year of simulation. The process builds the forecast by yearly time-steps
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'


ForecastDomPrices     <- function(DD,                       # D matrix for current scenario
                                  BaseL,
                                  PolInputs = MTI[[ss]],    # List of all elements detailing the policy applied
                                  ScenName  = ss,
                                  Year,
                                  LastHistYear){                    # Year of calculations (or tt if year-index)


    # Dummy to test: Year = BaseL$FirstModYear
    # Forecasting, year-by-year the different components that build up to the retail price
    tt            <- which(colnames(DD$p) == Year)
    t0            <- which(colnames(DD$p) == as.numeric(LastHistYear))



    ##########################################
    # FORECASTING ACCORDING TO CPAT FORMULAE #
    ##########################################

    # Forecasting is done in 2 stages:
    # 1. General formulae for all fuel/sectors
    # 2. Overwriting information for fuel/sectors that require particular treatments
    # test <- DD %>%
    #           select(CountryCode, SectorCode, FuelCode, IntPrices) %>%
    #           filter(is.na(IntPrices[,2]))
    #
    # unique(test$FuelCode)

    # Producer side subsidies [ps]
    DD$ps[,tt]    <- coalesce( DD$ps[,tt-1] * PolInputs$PhaseOut_ps[,tt],
                               # Replacing NAs with:
                               0 )


    # Pre-tax prices (supply-costs) [sp]
    # Supply costs are relevant for multiple other calculations. They should not be NA, nor 0.
    # The formula for the calculation is included within the rowSums function
    # This result is compared to 0.01 to select the max (I used cbind with 'Constant' and then apply a max by rows)
    # Finally, if any NA appears (unlikely given the rm.na = TRUE from the rowSums function), it will be replaced by a constant

    # --------------------------------------------------------------------------
    # NOTE:
    # The supply costs are the first source of multiple NAs or unexpected differences
    # Several changes have been implemented:
    #   - ratio between international prices is not done with the %/% operator, but instead within coalesce(ratio, 1)
    #   - the rowSums is allowed to yield NAs, which are then replaced by the lagged value of ps, or 0.01 ir that is missing

    # NOTE: Using here the %/% operator to exclude NAs from calculation of ratio.
    # This prevents the fuels for which we don't have international prices to yield crazy values
    DD$sp[,tt]    <- apply( cbind( coalesce( rowSums(cbind(DD$H2_FLsp,
                                                           (DD$sp[,tt-1] - DD$H2_FLsp) *
                                                              coalesce(DD$IntPrices[,tt] / DD$IntPrices[,tt-1] , 1),
                                                           (DD$ps[,tt] *
                                                              if_else(PolInputs$PhaseOut_ps[,tt] == 1, 0, 1))),
                                                     na.rm = F),
                                             # NOTE:
                                             # - The line above has been modified not to ignore NAs
                                             # - NAs will appear AT LEAST for fuels with no international prices
                                             # - The line below will replace those NAs with sp[,tt-1], or 0.01.
                                             # Replacing NAs with data from the previous period. If NA, replacing with 0.01:
                                             DD$sp[,tt-1],
                                             0.01),
                                   # Including a column of constant value to compute the max between both (Setting a lower bound)
                                   'Constant' = 0.01 ),
                            # To apply the operation by rows:
                            MARGIN = 1,
                            FUN = max)

    # VAT payment: General formulation, to be overwritten for special cases
    DD$vat[,tt]   <- coalesce( DD$vat[,tt-1] / (DD$sp[,tt-1] + DD$txo[,tt-1]*PolInputs$AddExternalityVAT) *
                                 (DD$sp[,tt-1] + DD$txo[tt]*PolInputs$AddExternalityVAT),
                               # Replacing NAs with:
                               0 )

    # Fixed, Ad Valorem, other taxes [fao]
    DD$fao[,tt]   <- coalesce( (DD$AdValFixed == 'Fixed')*rowMeans(DD$fao[,(t0-2):(t0)]) +
                                 ((DD$AdValFixed == 'Ad-Valorem')*(DD$fao[,t0]/DD$sp[,t0])*DD$sp[,tt-1])*
                                # TBC:
                                # This part below seems redundant, as it should always be = 1
                                # It suggests the current carbon prices should enter the equation
                                # Kept only for double-checking purposes
                                # NOTE:
                                # Before CPintroYear was used, as only 1 policy could be modelled at the time. Now fixed to CT.
                                if_else(((DD$fao[,t0] > 0) &
                                           ((ScenName != 'Scenario1')&
                                              (((tt > PolInputs$CTintroYear) |        # Modified this from CP to CT !
                                                (tt > PolInputs$ETSintroYear))))) ,   # Included ETS to complement
                                        # If true:
                                        as.numeric(PolInputs$ApplyExistingCP),
                                        # If false:
                                        1),
                                # Replacing NAs with:
                                0 )

    # Consumer side subsidy [cs]
    # NOTE: For the PCtrl_ coefficient, we can also use the bucketed parameter depending on the user's request
    DD$cs[,tt]      <- coalesce( apply( cbind(
                                    # The above apply and cbind work for the MIN function. The ones below, for the MAX
                                    apply( cbind( DD$H4 * PolInputs$PhaseOut_cs[,tt] +
                                                    (DD$PCtrl_alpha0-1)*
                                                    coalesce(DD$IntPrices[,tt] - DD$IntPrices[,tt-1], 0)*
                                                    PolInputs$PhaseOut_pc[,tt],
                                                  rowMeans(DD$cs[,c(t0:(t0-BaseL$BackYears+1))]) ),
                                           # To apply the operation by rows:
                                           MARGIN = 1,
                                           FUN = max),
                                    0),
                                    # To apply the operation by rows:
                                    MARGIN = 1,
                                    FUN = min),
                                 # Value to input if NAs result from the calculation
                                 0 )



    # DD$cs[,tt]      <- coalesce( DD$H4 * PolInputs$PhaseOut_cs[,tt] +
    #                                (DD$PCtrl_alpha0-1)*(DD$IntPrices[,tt] - DD$IntPrices[,tt-1])*PolInputs$PhaseOut_pc[,tt],
    #                              # Replacing NAs with:
    #                              0 )


    # Current carbon tax rate per unit of energy [xct]
    # Cov_sf would replace the current coverage_sector*coverage_fuel, allowing for more flexibility
    DD$xct[,tt]     <- coalesce( DD$XCT[,tt]*DD$ef*DD$XCTcov_sf[,tt],
                                 # Replacing NAs with:
                                 0 )

    # Existing ETS permit price per unit of energy [xets]:
    DD$xetsp[,tt]   <- coalesce( DD$XETSP[,tt]*DD$ef*DD$XETScov_sf[,tt],
                                 # Replacing NAs with:
                                 0 )

    # New carbon price [ncp]:
    DD$nct[,tt]     <- coalesce( DD$NCT[,tt]*DD$ef*PolInputs$NCTcov_sf[,tt],
                                 # Replacing NAs with:
                                 0 )

    # New excise tax [nxt]:
    DD$netsp[,tt]   <- coalesce( DD$NETSP[,tt]*PolInputs$NETScov_sf[,tt],
                                 # Replacing NAs with:
                                 0 )

    # Excise and other taxes [txo]:
    DD$txo[,tt]     <- rowSums( cbind( DD$fao[,tt], DD$cs[,tt], DD$xct[,tt], DD$xetsp[,tt], DD$nct[,tt], DD$netsp[,tt] ),
                                na.rm = TRUE )

    # Retail price:
    DD$p[,tt]       <- rowSums( cbind( DD$sp[,tt], DD$txo[,tt], DD$vat[,tt]),
                                na.rm = TRUE )


    # # TEST:
    # DD %>%
    #   filter(CountryCode == 'USA',
    #          FuelCode %in% c('coa', 'gso', 'nga', 'die', 'ele'),
    #          SectorCode %in% c('pow', 'res', 'srv', 'mch', 'ftr')) %>%
    #   select(CountryCode, SectorCode, FuelCode, p)




    return(DD)

}



#########################
## Mitigation Equation ##
#########################

#  This function:
#   - Calculates energy consumption for a given time step
#   - Reads the D matrix and fills the EC table for the selected year

#' Energy Use Equations
#'
#' @param DD Tibble with historical information and required structure in matrix column format (D-matrix)
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param PolInputs List of policy details to consider for the current scenario
#' @param Year Year of simulation. The process builds the forecast by yearly time-steps
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'


MitEQ  <- function(DD,                       # D matrix for current scenario
                   BaseL,
                   PolInputs = MTI[[ss]],    # List of all elements detailing the policy applied
                   Year){

    # Dummy to test: Year = BaseL$FirstModYear

    # Forecasting, year-by-year the different components that build up to the retail price
    tt            <- which(colnames(DD$p) == Year)
    t0            <- which(colnames(DD$p) == (BaseL$FirstModYear - 1))

    ##########################################
    # FORECASTING ACCORDING TO CPAT FORMULAE #
    ##########################################

    DD$ec[,tt]    <- DD$ec[,tt-1] *
                      (( 1 + DD$PostPolGDPgrowth[,tt]) ^ (DD$ElasticityIncome*DD$DiscFactorMitEQ )) *
                      ( 1 + PolInputs$ExogShockCOVID[,tt] ) * (( DD$p[,tt]/DD$p[,tt-1] )^DD$ElasticityOwnPriceUsage ) *
                      ((( DD$p[,tt]/DD$p[,tt-1] +
                            PolInputs$CovShadowPrice[,tt]*PolInputs$ShadowPrIncr[,tt] )^ DD$ElasticityOwnPriceEffic ) /
                      ( 1 + DD$EfficImprove + PolInputs$ExogShockCOVID[,tt-1]) ) ^ ( 1 + DD$ElasticityIncome)



    # TESTING:
    # varname       <- 'ec'
    #
    # AAA           <- DD %>%
    #                   select(CountryCode, FuelCode, SectorCode, varname) %>%
    #                   filter(FuelCode %in% c('coa'),
    #                          SectorCode %in% c('res'))
    # AAA
    #
    # AAA[[varname]] [,'2020']


    return(DD)
}



###################
## CO2 Emissions ##
###################

#  This function:
#   - Computes Emissions based on energy consumption, emission factors and a factor adjustment

#' CO2 Emissions calculation
#'
#' @param DD Tibble with historical information and required structure in matrix column format (D-matrix)
#' @param BaseL List with global parameters and templates to reconstruct matrices consistent with DD
#' @param PolInputs List of policy details to consider for the current scenario
#' @param Year Year of simulation. The process builds the forecast by yearly time-steps
#' @param AdjFactor Dummy constant to be replaced by country-specific data or by 1 (when using implicit EFs)
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'


Emissions   <- function(DD,                       # D matrix for current scenario
                        BaseL,
                        PolInputs = MTI[[ss]],    # List of all elements detailing the policy applied
                        Year,
                        AdjFactor = 1.12){


      tt            <- which(colnames(DD$p) == Year)

      DD$em[,tt]    <- coalesce(DD$ec[,tt], 0) * DD$ef * AdjFactor

      return(DD)
}
