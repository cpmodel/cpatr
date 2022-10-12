% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T04_DataInclExogProj.R
\name{PrepareInternationalPrices}
\alias{PrepareInternationalPrices}
\title{International Prices' source filtering}
\usage{
PrepareInternationalPrices(
  DD = DL$Scenario1,
  BaseL,
  IPList,
  SelSource = "IMF-IEA"
)
}
\arguments{
\item{DD}{Scenario-specific tibble with historical information and required structure in matrix column format (D-matrix)}

\item{BaseL}{List with global parameters and templates to reconstruct matrices consistent with DD}

\item{IPList}{List containing the international prices datasets by source}

\item{SelSource}{Selection of source for international prices data}
}
\description{
International Prices' source filtering
}