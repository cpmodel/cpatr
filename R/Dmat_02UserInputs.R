

###########################################################
##  MATRIX STRUCTURE GIVEN SELECTED COUNTRIES AND YEARS  ##
###########################################################

DefineCoverage      <- function(SelCountry   = CoveredCountryList$CountryCode,
                                FirstModYear = 2020,
                                MonBaseYear  = 2019,
                                BackYears    = 3,
                                LastModYear  = 2035,
                                LU           = Lookups){

    # Create a vector of all years
    AllYears        <- as.character((FirstModYear-BackYears):LastModYear)

    # Create a long tibble with all possible combinations of fuel sector code
    IDcols          <- expand.grid('CountryCode' = unique(SelCountry),
                                   'SectorCode'  = unique(LU$SectorGroups$SectorCode),
                                   'FuelCode'    = unique(LU$FuelTypes$FuelCode))

    # Creating a template matrix for expanded data (for each variable):
    TemplMat            <- matrix(0, nrow = dim(IDcols)[1], ncol = length(AllYears))
    colnames(TemplMat)  <- AllYears

    # Pack all this useful elements in a list:
    BaseList                <- list()
    BaseList$AllYears       <- AllYears
    BaseList$TemplMat       <- TemplMat
    BaseList$IDcols         <- IDcols
    BaseList$FirstModYear   <- FirstModYear
    BaseList$MonBaseYear    <- MonBaseYear
    BaseList$BackYears      <- BackYears
    BaseList$LastModYear    <- LastModYear
    BaseList$LU             <- LU
    BaseList$SelCountry     <- SelCountry



    return(BaseList)

}


######################
##  POLICY DETAILS  ##
######################

# This are currently detailed as it works for CPAT, assuming there would be 1 baseline and 1 policy scenario
# What is the best way to deal with scenarios?
#   - We could create a D matrix for each
#   - Create specific column names in the current D matrix by scenario
#   - rbind D to itself and add a column to specify in which scenario we are




