# Support functions for the test CPAT script


#########################################
#  CARBON PRICE TRAJECTORY (CT OR ETS)  #
#########################################

# This functions assumes the same policy is applied to all countries simultaneously
# The result is a matrix consistent with the D matrix structure, in which it will be included

#' Carbon price trajectory
#'
#' @param StartCPYear Starting year for the carbon price
#' @param TargetCPYear Year for the carbon price to reach the desired target level
#' @param StartCP Starting value for the carbon price
#' @param TargetCP Target value for the carbon price to reach by TargetCPYear
#' @param TemplMat Template matrix of 0s, with number of rows and columns consistent with the D matrix.
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'

CreateCP      <- function(StartCPYear  = 2023,
                          TargetCPYear = 2030,
                          StartCP      = 60,
                          TargetCP     = 100,
                          TemplMat){

      CP              <- TemplMat
      YearVector      <- colnames(TemplMat)

      # Positions within which carbon price will be interpolated
      Yst             <- YearVector >= StartCPYear
      Yend            <- YearVector <= TargetCPYear

      # Linear interpolation from starting year to target year using user-defined carbon prices or ETS permit prices
      CP[,Yst & Yend] <- matrix(rep(approx(x = c(StartCP, TargetCP), n = (TargetCPYear-StartCPYear) + 1, rule = 2)$y,
                                    dim(CP)[1]),
                                nrow = dim(CP)[1], ncol = (TargetCPYear-StartCPYear) + 1, byrow = TRUE)

      # Assuming CP remains the same after target year
      CP[,!Yend]      <- TargetCP

      # Returning the matrix with the CP trajectory expanded for all countries, sectors and fuels
      return(CP)

}


########################################################
#  CONVERTING A D-MATRIX TYPE OF OBJECT INTO A TIBBLE  #
########################################################

# This function:
#   - Performs the equivalent of a pivot_longer on a tibble built with matrix columns

#' Convert matrix column into longer tibble
#'
#' @param DD Tibble using matrix columns (D matrix) to be transformed into longer format
#' @param ColCateg Name of the variable that will store the current column names of DD
#' @param ReplColCateg Entry to replace the ColCateg for vectors of DD that are not matrix columns
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'

Convert_D_to_T      <- function(DD,
                                ColCateg     = 'Year',
                                ReplColCateg = 'Constant'){

    # ------------------------------------------------------------
    # This function takes a D matrix as input and returns a tibble after pivoting longer
    # D matrices have matrices within its columns. Those matrices may have different categories as columns (time series vs IOT)
    # For the current version, we only have time as category for all the cases of matrix columns.
    # The user decides the name of the variable to absorb those categories when pivoting longer (Default: ColCateg = 'Year')
    # For constants/parameters, the variable will have a single column.
    # To keep consistency, two alternatives are considered. The second was taken:
    #   - Replicate the constant for all years and create an entry for each one under the column 'Year' on the output tibble
    #   - Create a user-defined entry for the column Year whenever the variable is time-constant. Year = 'constant' for those rows.
    #       - ReplColCateg is the character string that will be used as an entry under the 'Year' column in those cases


    # The function pivots longer each individual variable, and then stacks one after the other.
    # To be consistent, they should have the same columns.
    # Character columns are all assumed to be IDcols, even if this doesn't match with 'true' IDcols
    # ------------------------------------------------------------

    # Names of all columns in D matrix. Some of these will be matrices themselves
    NamesD          <- names(DD)

    # Numeric column names (either vectors or matrix columns)
    NumCols         <- DD %>%
                        select_if(is.numeric) %>%
                        names()

    # Character columns will most likely by ID cols.
    ChrCols         <- NamesD[!NamesD %in% NumCols]

    # Initialize the D matrix in tibble format Dtib
    Dtib            <- tibble()

    # ------------------ #
    # Conversion process #
    # ------------------ #

    # Applied column by column (on numeric columns only)
    # Matrices and vectors are treated separately
    # The resulting tibble is obtained after binding by rows all results


    for(nn in NumCols){ # nn <- NumCols[1]

        # Each variable is converted into a longer tibble before binding it to the general one.
        # The dimensions of the data object are used to differentiate matrices from vectors
        if(dim( as.matrix( DD[[nn]] ) )[2] > 1){

            # Process for matrices
            DataMat     <- DD[[nn]]
            CurrentD    <- cbind(DD %>% select(all_of(ChrCols)),
                                 # Including a column to register the name of each variable:
                                 'Variable' = nn,
                                 # Adding the data columns:
                                 DataMat) %>%
                            pivot_longer(-c(all_of(ChrCols), 'Variable'), names_to = ColCateg, values_to = 'Value')

        }else{

            # Process for vectors
            DataVec     <- DD[[nn]]
            CurrentD    <- cbind(DD %>% select(all_of(ChrCols)),
                                 tibble('Variable'   = nn,                 # Column with variable's name
                                        '{ColCateg}' := ReplColCateg,      # Default: Year = ... (= 'Constant' for parameters)
                                        'Value'      = DataVec))           # The numeric vector with relevant data

        }

        # Binding the new table to the general one
        Dtib            <- Dtib %>%
                            rbind(CurrentD)

    }

    # Returning the data in long format
    return(Dtib)

}

# Testing <- Convert_D_to_T()
#
# unique(Testing$Variable)
#
# saveRDS(Testing, 'integration/ExampleDtoTibble.RDS')
# dim(Testing)


#########################################
#  CONVERTING A TIBBLE INTO A D MATRIX  #
#########################################

# This process is simpler but has much more variations to consider and needs to be more flexible
# The current version assumes a single variable is given, and transforms it into a D matrix format

#' Convert dataframe in long format into tibble with matrix columns
#'
#' @param TT Tibble in longer format, to convert into a tibble with matrix columns
#' @param ColVarNames Column in TT where the variable names are stored
#' @param NamesToWide Column in TT from which to read the names for the columns within each matrix column
#' @param ValuesToWide Column in TT where the variable values are stored
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'

Convert_T_to_D      <- function(TT,
                                ColVarNames  = 'Variable',    # Column storing the variable's names
                                NamesToWide  = 'Year',        # Name of column in TT with the (column) names for pivot_wider
                                ValuesToWide = 'Value'){      # Name of column in TT with the (column) values for pivot_wider


    # --------------------------------------------------------------------------
    # The tibble contains ID variables, and other columns useful for the application of a pivot_wider (case-by-case)
    # The ID variables will most likely be character cols and should be the same for all years (assuming year as col categ)
    # For the other columns, the tibble will need to read some of them as vectors and some as matrices when applying the pivot_wider
    # For the matrices:
    #     - values_from will be taken from the column in TT named as specified in ValuesToWide (default: 'Value')
    #     - names_from will be taken from the column in TT named as specified in NamesToWide (default: 'Year')
    # For vectors:
    #     - A unique column is passed on to the new D, but the name is taken from the proper colum in TT (ColVarNames)
    # --------------------------------------------------------------------------


    # Names of the numeric variables that will be considered for the pivot_wider
    # These names are currently stored in the Tibble under the column called (ColVarNames = 'Variable')
    NamesT          <- unique(TT[[ColVarNames]])

    # Names of columns that will be used as ID (ideally, character)
    IDcolNames      <- names(TT)[! names(TT) %in% c(ColVarNames, NamesToWide, ValuesToWide)]

    # NOTE:
    #   - This may not be robust enough if applied to other more general cases
    #   - The order of rows most likely differs with respect to any original D matrix due to distinct(), which sorts rows
    IDcols          <- TT %>%
                        select(IDcolNames) %>%
                        distinct()

    # Initializing the resulting D matrix
    OutD            <- IDcols


    # Adding each variable as a matrix column
    for(nn in NamesT){ # nn <- NamesT[2]

        # Number of columns (default: number of years) to see if nn is constant in time
        # NOTE:
        # Passing a dynamic name to the filter using !!(sym()). See more:
        # https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
        NumOfCols   <- TT %>%
                        dplyr::filter( !!sym(ColVarNames)==nn ) %>%
                        select(!!!NamesToWide) %>% unique() %>%
                        unlist() %>%
                        length()

        if(NumOfCols > 1){

            # Case for matrices

            TempTib     <- TT %>%
                            # !!sym() evaluates the ColVarNames. Searches for it among the colnames of TT. Filters the elements == nn
                            dplyr::filter( !!sym(ColVarNames)==nn ) %>%
                            pivot_wider(names_from = NamesToWide, values_from = ValuesToWide) %>%
                            # NOTE: for select() there is no need for evaluation or for transformation to symbols
                            select(- ColVarNames)

            # Splitting the data from the ID columns:
            DataMat     <- as.matrix(TempTib %>%
                                       select(-IDcolNames))

            # Sub matrix to be joined to the general one
            # Recall:
            #   - '{nn}' := evaluates nn and assigns that name to the column to which we will input the new data matrix
            SubD        <- tibble(TempTib %>% select(IDcolNames),
                                  '{nn}' := DataMat)


        }else{

            # Case for vectors
            TempTib     <- TT %>%
                            # !!sym() evaluates the ColVarNames and searches for it among the colnames of TT, then filter the elements == nn
                            dplyr::filter( !!sym(ColVarNames)==nn ) %>%
                            pivot_wider(names_from = ColVarNames, values_from = ValuesToWide) %>%
                            # NOTE: for select() there is no need for evaluation or for transformation to symbols
                            select(- ColCateg)

            # Sub matrix to be joined to the general one
            # Recall:
            #   - '{nn}' := evaluates nn and assigns that name to the column to which we will input the new data matrix
            SubD        <- tibble(TempTib %>% select(IDcolNames),
                                  '{nn}' := TempTib[[nn]])

        }

        # Joining the newly created (matrix) column to the output D
        OutD        <- OutD %>%
                        left_join(SubD, by = names(IDcols))

    }

    # Output D
    # NOTE:
    #   - The order of rows may differ with original D matrix (if any) due to the use of distinct() when creating the IDcols
    #   - IDcols here may include other (non-numeric) columns aside from those named as such in the original D matrix (if any)
    return(OutD)

}



#####################################
#  COUNTRY SELECTION AND FILTERING  #
#####################################

#' This function:
#' - Filters historic data to keep only that relevant for the selected countries
#' - It adapts templates and elements from the BaseList to match the new dimensions needed
#'
#' @param DD Tibble using matrix columns (D matrix) to be filtered
#' @param FullBaseL List with pre-defined defaults and templates
#' @param SelectedCountryList List of selected countries
#' @param LocalUserScen List of all inputs from user, except the Countries selected
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'

FocusCountry        <- function(DD,
                                FullBaseL,
                                SelectedCountryList,
                                LocalUserScen){


      # This is the number of unique rows needed for each country, and that will be used for the templates
      SC_NumRows        <- (FullBaseL$IDcols %>%
                              select(-CountryCode) %>%
                              distinct() %>%
                              dim(.) )[1]

      SelectionNumRows  <- length(SelectedCountryList) * SC_NumRows


      # ---------------------------------------- #
      # Adjusting BaseList to the new dimensions
      # ---------------------------------------- #

      # Initializing the Base List that will be returned to the user
      FilterBaseL       <- FullBaseL

      # Adjusting the relevant entries of the list
      FilterBaseL$IDcols      <- FullBaseL$IDcols %>%
                                  dplyr::filter( CountryCode %in% SelectedCountryList )
      FilterBaseL$SelCountry  <- SelectedCountryList

      # Adapting the template matrix for the number of countries selected:
      FilterBaseL$TemplMat    <- FullBaseL$TemplMat[1:SelectionNumRows,]



      # ------------------------------------------------------- #
      # Adjusting the Baseline parameters to the new dimensions #
      # ------------------------------------------------------- #

      # These depend on country selection, so they need to be adjusted
      BaselineParams      <- BaselineInputs(BaseL = FilterBaseL )[[1]]


      # Baseline policy coverage for new carbon tax: a matrix of zeros whose dimensions depend on the number of countries chosen
      # This is used as reference to build the matrix of policy coverage given user's selection of sectors and fuels
      BaselineCoverage    <- tibble( FilterBaseL$IDcols %>%
                                       cbind( BaselineParams$NCTcov_sf ))


      # ----------------------------------------------- #
      # Creating a consistent list of policy parameters #
      # ----------------------------------------------- #

      # Carbon prices are straightforward to input, but coverage is not.
      # The coverage matrix needs to be computed based on the country, fuel and sector selection by the user
      NCTcov_sf           <- BaselineCoverage %>%
                              pivot_longer(cols = -c(CountryCode,
                                                     SectorCode,
                                                     FuelCode),
                                           names_to = "Year",
                                           values_to = "Value") %>%
                              mutate(Value = if_else(CountryCode %in% SelectedCountryList &
                                                       SectorCode %in% LocalUserScen$SelectedSectors &
                                                       FuelCode %in% LocalUserScen$SelectedFuels &
                                                       Year >= LocalUserScen$CTintroYear,
                                                     1,
                                                     0) ) %>%
                              pivot_wider(names_from = "Year", values_from = 'Value') %>%
                              select(-c(CountryCode, SectorCode, FuelCode)) %>%
                              as.matrix()


      # With the coverage and the inputs provided by the user, the list of parameters as required by the model can be built
      PolicyParams        <- list("IntPricesSource" = BaselineParams$IntPricesSource,
                                  "AddExternalityVAT" = BaselineParams$AddExternalityVAT,
                                  "CTintroYear" = LocalUserScen$CTintroYear,
                                  "CTintroValue" = LocalUserScen$CTintroValue,
                                  "CTtargetYear" = LocalUserScen$CTtargetYear,
                                  "CTtargetValue" = LocalUserScen$CTtargetValue,
                                  "NCTcov_sf" = NCTcov_sf,
                                  "ETSintroYear" = BaselineParams$ETSintroYear,
                                  "ETSintroValue" = BaselineParams$ETSintroValue,
                                  "ETStargetYear" = BaselineParams$ETStargetYear,
                                  "ETStargetValue" = BaselineParams$ETStargetValue,
                                  "NETScov_sf" = BaselineParams$NETScov_sf,
                                  "ApplyExistingCP" = FALSE,
                                  "PhaseOut_cs" = BaselineParams$PhaseOut_cs,
                                  "PhaseOut_ps" = BaselineParams$PhaseOut_ps,
                                  "PhaseOut_pc" = BaselineParams$PhaseOut_pc,
                                  "ShadowPrIncr" = BaselineParams$ShadowPrIncr,
                                  "CovShadowPrice" = BaselineParams$CovShadowPrice,
                                  "ExogShockCOVID" = BaselineParams$ExogShockCOVID)


      # ----------------------------------------------- #
      # Creating a list of processed elements to return #
      # ----------------------------------------------- #

      TreatedList         <- list()
      TreatedList$BaseL   <- FilterBaseL
      TreatedList$BaselineParams  <- BaselineParams
      TreatedList$PolicyParams    <- PolicyParams

      return(TreatedList)

}





