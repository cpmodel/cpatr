
options(scipen = 999)
# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(httr)
library(purrr)
library(reactable)

# Load static data
countries <- read.csv('countries.csv')

# Variables
simplified_report <- TRUE
df <- midwest

