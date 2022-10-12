#' BaseList_AllCountries
#' A list providing information on base variables not currently changeable by the user. Covers all countries.
#' These variables determine the dimensions of the data structures, as they provide starting year, end year,
#' number of countries, among others.
#' The list also includes an empty template matrix that is used to create the new matrix columns whenever needed.
#' @format List
'BaseList_AllCountries'

#' DB_AllCountries
#' An array including matrix columns for all countries and years selected by default.
#' This dataset contains the historical information for key variables that do not depend on user-defined parameters
#' and provides space ready-to-be-filled for forecasts.
#'  @format Tibble, array. It includes single columns to be read as column vectors, and also others as matrix columns.
'DB_AllCountries'

#' RInputs_RawGDPRelativeToBase
#' Dataset with information on the real GDP in each year over that of the base year. Used to compute Energy Consumption
#' @format tibble
'RInputs_RawGDPRelativeToBase'

#' RInputs_InternationalPrices_IntPrices
#' @format tibble
'RInputs_InternationalPrices_IntPrices'

#' RInputs_InternationalPrices_RegionAssumptions
#' @format tibble
'RInputs_InternationalPrices_RegionAssumptions'

#' RInputs_InternationalPrices_RegionMarket
#' @format tibble
'RInputs_InternationalPrices_RegionMarket'


