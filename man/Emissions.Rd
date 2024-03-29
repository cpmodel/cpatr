% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T02_CPATCoreFunctions.R
\name{Emissions}
\alias{Emissions}
\title{CO2 Emissions calculation}
\usage{
Emissions(DD, BaseL, PolInputs = MTI[[ss]], Year, AdjFactor = 1.12)
}
\arguments{
\item{DD}{Tibble with historical information and required structure in matrix column format (D-matrix)}

\item{BaseL}{List with global parameters and templates to reconstruct matrices consistent with DD}

\item{PolInputs}{List of policy details to consider for the current scenario}

\item{Year}{Year of simulation. The process builds the forecast by yearly time-steps}

\item{AdjFactor}{Dummy constant to be replaced by country-specific data or by 1 (when using implicit EFs)}
}
\description{
CO2 Emissions calculation
}
