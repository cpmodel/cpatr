# rm(list=ls())

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#########################
#  INITIALIZING CPAT R  #
#########################

# source('R/T01_ConfigSimplifiedCPAT.R')



#########################################################################
#  SIMPLIFIED VERSION OF CPAT: MITIGATION EQUATION AND PRICE ALGORITHM  #
#########################################################################

#' CPAT R v0.0.0
#'
# @param DB Default dataset with historical information and required format
# @param BaseList List with global parameters and templates to reconstruct matrices consistent with DD
# @param MTI List of scenarios, with the inputs to use in each.
#'
#' @return
#' @export
#' @import tidyr dplyr purrr tidyverse readxl readr stringr
#'
#' @examples

SimpleCPAT        <- function(HistoricDataset,
                              BaseList,
                              UserScen){



    # Initializing the Scenarios:
    MTI               <- list()
    MTI$Scenario1     = BaselineInputs(BaseList)[[1]]
    MTI$Scenario2     = UserScen


    #------------------------------------------------------------------------------------------------------------#
    # Including data that has both historical information and projections, but does not depend on user's choices #
    #------------------------------------------------------------------------------------------------------------#

    # GDR relative to base (equal for all scenarios)
    DB            <- PrepareGDPRelativeToBase(DD = HistoricDataset,
                                              BaseL = BaseList,
                                              RawGDPRelativeToBase = RInputs_RawGDPRelativeToBase)



    # List of datasets: One dataset for each scenario.
    # They can be later on combined with and rbind, by including an extra column to signal the scenario
    DL              <- list()

    ScenarioNames   <- names(MTI)


    for(ss in ScenarioNames){ # ss <- ScenarioNames[1]

        # Initializing the data for the scenario
        DL[[ss]]    <- DB

        #------------------------------------------------------------------------------------------#
        # Variables with historical data and projections, where the latter depend on user's inputs #
        #------------------------------------------------------------------------------------------#

        # Data on international prices, after the user has selected a data source
        DL[[ss]]    <- PrepareInternationalPrices(DD        = DL[[ss]],
                                                  BaseL     = BaseList,
                                                  IPList    = PrepocessIntPricesList(BaseList,
                                                                                     RInputs_InternationalPrices_RegionAssumptions,
                                                                                     RInputs_InternationalPrices_RegionMarket,
                                                                                     RInputs_InternationalPrices_IntPrices),
                                                  SelSource = MTI[[ss]]$IntPricesSource)


        #---------------------------------------------------------------------------------------#
        # Dummy data for additional variables/parameters required:                              #
        #---------------------------------------------------------------------------------------#

        # -----
        # Post-Pol GDP growth (scenario-dependent)
        # Proper calculations require the calculations of revenues, revenue recycling and the computing if their effect through multipliers
        tempDummy                       <-DL[[ss]]$GDPFactor[,-1] / DL[[ss]]$GDPFactor[,-dim(DL[[ss]]$GDPFactor)[2]] - 1

        # Including an extra column for the first year, and replacing the first column ('Inf') by '0':
        tempDummy[,1]                   <- 0
        tempDummy                       <- cbind(0, tempDummy)
        colnames(tempDummy)             <- colnames(DL[[ss]]$GDPFactor)
        DL[[ss]]$PostPolGDPgrowth       <- tempDummy


        # -----
        # Discount factor (proportion of estimated to average calibrated income elasticity)
        # Used in the mitigation equation. Relies on GDP post policy, so fixed to 1 for now
        DL[[ss]]$DiscFactor             <- 1


        #---------------------------------------------------------------------------------------#
        # Reading the policy inputs                                                             #
        #---------------------------------------------------------------------------------------#

        # Scenario1 is the baseline
        if(ss == ScenarioNames[1]){

            # New carbon tax under the baseline
            DL[[ss]]$NCT        <- BaseList$TemplMat
            # Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
            DL[[ss]]$NCTcov_sf  <- BaseList$TemplMat

            # New ETS under the baseline
            DL[[ss]]$NETSP      <- BaseList$TemplMat
            # Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
            DL[[ss]]$NETScov_sf <- BaseList$TemplMat

        }else{

            # New Carbon tax under the policy (assumed equal for all countries)
            DL[[ss]]$NCT        <- CreateCP(StartCPYear  = MTI[[ss]]$CTintroYear,
                                            TargetCPYear = MTI[[ss]]$CTtargetYear,
                                            StartCP      = MTI[[ss]]$CTintroValue,
                                            TargetCP     = MTI[[ss]]$CTtargetValue,
                                            TemplMat     = BaseList$TemplMat)
            # Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
            DL[[ss]]$NCTcov_sf  <- MTI[[ss]]$NCTcov_sf

            # New ETS permit under the policy
            DL[[ss]]$NETSP      <- CreateCP(StartCPYear  = MTI[[ss]]$ETSintroYear,
                                            TargetCPYear = MTI[[ss]]$ETStargetYear,
                                            StartCP      = MTI[[ss]]$ETSintroValue,
                                            TargetCP     = MTI[[ss]]$ETStargetValue,
                                            TemplMat     = BaseList$TemplMat)
            # Initializing the coverage, as it will still be re-written in the price algorithm (redundant?)
            DL[[ss]]$NETScov_sf <- MTI[[ss]]$NETScov_sf
        }


        #---------------------------------------------------------------------------------------#
        # Mitigation equation and Price algorithm: iterations by time step                      #
        #---------------------------------------------------------------------------------------#

        for(yy in c(BaseList$FirstModYear:BaseList$LastModYear)){  # yy <- BaseList$FirstModYear

            # Applying the price algorithm to the current data
            DL[[ss]]          <- ForecastDomPrices(DD        = DL[[ss]],           # D matrix for the current scenario
                                                   BaseL     = BaseList,
                                                   PolInputs = MTI[[ss]],          # List of all elements detailing the policy applied
                                                   ScenName  = ss,
                                                   Year      = yy)                 # Year of calculations (or tt if year-index)

            # Applying the mitigation equation on top of the newly computed prices
            DL[[ss]]          <- MitEQ(DD                    = DL[[ss]],           # D matrix for current scenario
                                       BaseL                 = BaseList,
                                       PolInputs             = MTI[[ss]],          # List of all elements detailing the policy applied
                                       Year                  = yy)

            # Computing Emissions
            # Note:
            #   - The function has been written to apply it year by year, but it could be applied to the full matrix afterwards
            DL[[ss]]          <- Emissions(DD                = DL[[ss]],           # D matrix for current scenario
                                           BaseL             = BaseList,           # Unused here
                                           PolInputs         = MTI[[ss]],          # Unused here
                                           Year              = yy,
                                           AdjFactor         = 1.12)
        }

        # TESTING: Un-comment and select the variable to focus on
        # varname       <- 'ec'
        #
        # AAA           <- DL[[ss]] %>%
        #                   select(CountryCode, FuelCode, SectorCode, varname) %>%
        #                   filter(FuelCode %in% c('coa'),
        #                          SectorCode %in% c('res'))
        # AAA
        #
        # AAA[[varname]] [,'2020']



    }


    return(DL)
}


# # Test:
# TestCPAT      <- SimpleCPAT()
# TestTibble    <- Convert_D_to_T(TestCPAT$Scenario1)
#
# saveRDS(TestCPAT, 'R/ExampleOutput.RDS')
# saveRDS(TestTibble, 'R/BaselineExampleLongTibble.RDS')

# varnames      <- c('ElasticityOwnPriceUsage', 'ElasticityOwnPriceEffic', 'ElasticityIncome', 'ec')
# ss            <- 'Scenario1'
# cc            <- c('USA')
# ff            <- c('coa', 'gso', 'nga', 'die', 'ele')
# sect          <- c('pow', 'res', 'srv', 'mch', 'ftr')
#
#
# AAA           <- TestCPAT[[ss]] %>%
#                   select(CountryCode, FuelCode, SectorCode, all_of(varnames)) %>%
#                   filter(CountryCode %in% cc,
#                          FuelCode %in% ff,
#                          SectorCode %in% sect)

#test1 <- CodedCPAT::SimpleCPAT()
