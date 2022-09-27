# 1. API CALL: Providing a) Function Name b) Input codes (country, sectors and fuel types) and c) Table of model configuration
# 2. Extract from the API call the model configuration and select the carbon tax created by that model configuration.
# 3. FILTER by JOINING D with the required input codes.

#' Title
#'
#' @param start_year Start year of calculations: including the t=0
#' @param end_year End year of calculations
#' @param params_input Vector of codes
#' @param values_input Vector of initial inputs
#' @param D_matrix Tibble Matrix with required information
#'
#' @return
#' @export
#' @import tidyr dplyr purrr
#'
#' @examples
mitigation_function <- function(start_year = 2019,
                                end_year = 2035,
                                params_input = NA,
                                values_input = NA,
                                D_matrix
){

  if(length(params_input) == 1){
    params_input_c <- params_input %>%
      strsplit(., '.', fixed = T) %>%
      purrr::reduce(rbind) %>%
      # tibble::enframe() %>%
      t() %>%
      as.data.frame() %>%
      select(CountryCode  = 1, SectorCode  = 5, FuelType  = 4) %>%
      mutate(CountryCode  = toupper(CountryCode)) %>%
      mutate(Code = params_input) %>%
      mutate(Values = values_input) #initial values code
  }else{
    # split codes into clean params necessary to subset D matrix
    params_input_c <- params_input %>%
      strsplit(., '.', fixed = T) %>%
      purrr::reduce(rbind) %>%
      as.data.frame() %>%
      select(CountryCode  = 1, SectorCode  = 5, FuelType  = 4) %>%
      mutate(CountryCode  = toupper(CountryCode)) %>%
      mutate(Code = params_input) %>%
      mutate(Values = values_input) #initial values code
  }

  D <- D_matrix %>%
    filter(SectorCode %in% params_input_c$SectorCode) %>%
    filter(FuelType %in% params_input_c$FuelType) %>%
    filter(CountryCode %in% params_input_c$CountryCode) %>%
    left_join(params_input_c)

  # overwrite if necessary
  if(!(length(values_input) == 1 && is.na(values_input))){
    D$ec[,1] = D$Values
  }

#REPLACE WITH THE MITIGATION EQUATION
  #Calculate the ec for each year
  for(i in 1:(length(start_year:end_year)-1)) {
    cur_year <- (start_year:end_year)[i] # current year
    D$ec[,i+1] = D$ec[,i]*D$cov_adj*((D$gdp[,i+1]/D$gdp[,i])^D$el_inc)*((1/(1+D$eff))^(1+D$el_dem))*((D$atp[,i+1]/D$atp[,i])^D$el_pr)
  }

  # subset columns to required years only
  # D$ec is a matrix, using base R
  colz <- (which(colnames(D$ec) %in% start_year:end_year))
  res <- D$ec[,colz]

  cn <- colnames(res)
  res <- as.data.frame(res)
  names(res) <- cn
  res <- cbind(Code = paste(D$Code), res)
  res <- res %>% pivot_longer(-Code) %>% as.list()

  res

}
