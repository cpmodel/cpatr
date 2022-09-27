#-------------------------------------------------------------------------------
# Name:ME4_UserLevelFunctions.R
# Purpose: Script containing at the user level, after pre-processing is finished

# Current Functions:
#  - CreateCT: Creates a vector with the evolution of the carbon tax per year
#  - MitEQ: Runs the mitigation equation to project energy consumption


# Change Log
# Version   Initials  Date          Change
# V1        DB        2022-Feb-24   Creation of file
# V2        AA/SJS    2022-May-06   Major update and production
#-------------------------------------------------------------------------------




##CARBON TAX##
# This function:
#  - Creates a liner interpolation of Carbon Tax based on user defined parameters
#  - TBC: behaviour of series after data of achievement of target
# - CPAT currently keeps the trend.
# - This alternative keeps the target constant after the desired year

CreateCT <- function(StartCTYear = 2022,
                     TargetCTYear = 2030,
                     StartCT = 60,
                     TargetCT = 100,
                     InitYear = StartYear,
                     FinYear = EndYear) {

  NumYears <- FinYear - InitYear + 1
  YearVector <- InitYear:FinYear

  # Creating an empty vector to fill with carbon tax information
  CT <- rep(0, NumYears)

  # Positions within which carbon tax will be interpolated
  Yst <- YearVector >= StartCTYear
  Yend <- YearVector <= TargetCTYear

  # Linear interpolation from starting year to target year using user-defined carbon taxes
  CT[Yst & Yend] <- approx(x = c(StartCT, TargetCT), n = (TargetCTYear-StartCTYear) + 1, rule = 2)$y
  # Assuming CT remains the same after target year
  CT[!Yend] <- TargetCT

  CTmatrix <- matrix(CT, nrow = 1, ncol = length(CT))
  colnames(CTmatrix) <- YearVector
  CarbonTax <- tibble(ct = CTmatrix)

  return(CarbonTax)
}

#' Title AddPricesToDMatrixAndUpdateRP
#'
#' @param D A tibble with matrix-columns object
#'
#' @return D A tibble with matrix-columns object
#' @export
#'
#' @examples
AddPricesToDMatrixAndUpdateRP <- function(D, PolicyVariable) {
  #for() {

  D <- D %>%
    mutate(rp_B = 123) # Add your code here

  #}
  return(D)
}

#D = D %>% AddPricesToDMatrixAndUpdateRP() #Use Case

#' Title AddCarbonTaxToPrices
#'
#' @param D A tibble with matrix-columns object
#'
#' @return D A tibble with matrix-columns object
#' @export
#'
#' @examples
AddCarbonTaxToPrices <- function(D) {

  # check 1 > is this a tibble matrix col
  # check 2 > does it contain columns rp, ec, ef
  # check 3 > are these matrices
  # check 4 > they aren't empty with all NA
  # ....

  D <- D %>%
    mutate(rp_P = rp_B + ct*ef)

  return(D)
}


#########################
## ENERGY USE EQUATION ##
#########################

#' Energy Use Equations
#'
#' @param InitY Start year of calculations: including the t=0
#' @param EndY End year of calculations
#' @param TempD Tibble Matrix with required information
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'
#' @examples
MitEQ  <- function(TempD = D,
                   InitY = StartYear,
                   EndY = EndYear){

  for(i in (1:(length(StartYear:EndYear) - 1))){ #i<-1
    TempD$ec[,i + 1]<- TempD$ec[,i] * TempD$cov_adj * ((TempD$gdp[,i+1] / TempD$gdp[i])^TempD$el_inc)*
      ((1/(1 + TempD$eff))^(1 + TempD$el_dem))*((TempD$atp[,i + 1] / TempD$atp[i])^TempD$el_pr)
  }

  return(TempD)
}


#' Title
#'
#' @param Dint
#' @param InitY
#' @param EndY
#'
#' @return
#' @export
#'
#' @examples
UpdatesAllDataPoints <- function(Dint = D,
                                 InitY = StartYear,
                                 EndY = EndYear) {

  years_len <- length(StartYear:EndYear) - 1

  for(i in (1:years_len)){

    #PricesIncrmentT(D,....)
    #Pseudo code in progress...

  }
  return(output)

}


#' Title
#'
#' @param Dint
#' @param InitY
#' @param EndY
#'
#' @return
#' @export
#'
#' @examples
UpdatesAllDataPoints <- function(Dint = D,
                                 InitY = StartYear,
                                 EndY = EndYear) {

  years_len <- length(StartYear:EndYear) - 1

  for(i in (1:years_len)) {

    EnergyUseTminusonetoT(D = D,
                          InitY = StartYear,
                          EndY = EndYear,
                          i = i)

    # function to be created, for demo purposes
    #MacroGDPEndogenousIncrementT(D....)

  }

}



#' Title
#'
#' @param D
#' @param InitY
#' @param EndY
#' @param i
#'
#' @return
#' @export
#'
#' @examples
EnergyUseTminusonetoT  <- function(D = D,
                                   InitY = StartYear,
                                   EndY = EndYear,
                                   i = 1){

  D$ec[,i + 1]<- D$ec[,i] * D$cov_adj * ((D$gdp[,i+1] / D$gdp[i])^D$el_inc) *
    ((1/(1 + D$eff))^(1 + D$el_dem)) * ((D$atp[,i + 1] / D$atp[i])^D$el_pr)

  return(D)
}

