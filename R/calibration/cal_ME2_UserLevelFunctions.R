#-------------------------------------------------------------------------------
# Name:     ME4_UserLevelFunctions.R
# Purpose:  Script containing at the user level, after pre-processing is finished

# Current Functions:
#   - CreateCT: Creates a vector with the evolution of the carbon tax per year
#   - MitEQ: Runs the mitigation equation to project energy consumption


# Change Log
# Version   Initials    Date        Change
# 0.1       DB         24-Feb-22    Creation
#-------------------------------------------------------------------------------



########################
##     CARBON TAX     ##
########################

#  This function:
#   - Creates a liner interpolation of Carbon Tax based on user defined parameters
#   - TBC: behaviour of series after data of achievement of target
#       - CPAT currently keeps the trend.
#       - This alternative keeps the target constant after the desired year

CreateCT        <- function(StartCTYear  = 1990,
                            TargetCTYear = 2019,
                            StartCT      = 0,
                            TargetCT     = 0,
                            InitYear     = StartYear,
                            FinYear      = EndYear) {

    NumYears    <- FinYear-InitYear+1
    YearVector  <- InitYear:FinYear

    # Creating an empty vector to fill with carbon tax information
    CT          <- rep(0, NumYears)

    # Positions within which carbon tax will be interpolated
    Yst         <- YearVector >= StartCTYear
    Yend        <- YearVector <= TargetCTYear

    # Linear interpolation from starting year to target year using user-defined carbon taxes
    CT[Yst & Yend]  <- approx(x = c(StartCT, TargetCT), n = (TargetCTYear-StartCTYear)+1, rule = 2)$y
    # Assuming CT remains the same after target year
    CT[!Yend]       <- TargetCT

    CTmatrix        <- matrix(CT, nrow = 1, ncol = length(CT))
    colnames(CTmatrix) <- YearVector
    CarbonTax       <- tibble(ct = CTmatrix)

    return(CarbonTax)
}


#########################
## ENERGY USE EQUATION ##
#########################

MitEQ           <- function(TempD = D,
                            InitY = StartYear,
                            EndY  = EndYear){

    numYears    <- length(StartYear:EndYear)-1

    for(i in (1:numYears)) { # i <- 1
        TempD$ec[,i+1]      <-  TempD$ec[,i] * TempD$cov_adj * ( (TempD$gdpreltobase[,i+1] / TempD$gdpreltobase[,i])^TempD$el_inc )*
                                ( ( 1/(1+TempD$eff) )^(1+TempD$el_dem) )*( (TempD$atp[i+1] / TempD$atp[i])^TempD$el_pr)

        # Test_dat            <- TempD %>%
        #                         filter(CountryCode %in% c('USA', 'BRA', 'ZAF'),
        #                                SectorCode == 'pow',
        #                                FuelType == 'coa')
        #
        # print(Test_dat)
    }

    return(TempD)
}

