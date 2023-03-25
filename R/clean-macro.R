
# Clean Macro Data --------------------------------------------------------

# keep it tidy
library(tidyverse)
library(dplyr)
library(stringr)

# load data
macro = readr::read_csv("I:/datathon-data/macro_data.csv") # jack's path

# reformat column names to make them easier to understand
for(i in 2:ncol(macro)) {
  names(macro)[i] = paste(str_extract_all(macro[2,i], boundary("word"))[[1]][1],
                          str_extract_all(macro[2,i], boundary("word"))[[1]][2],
                          str_extract_all(macro[2,i], boundary("word"))[[1]][3],
                          str_extract_all(macro[2,i], boundary("word"))[[1]][4],
                          sep = "_")
}
names(macro)[1] = "Date"
macro = macro[-c(1:7),]

# make columns numeric
macro[,2:ncol(macro)] = sapply(macro[,2:ncol(macro)], function(x) as.numeric(x))

# select the relevant columns and reformat date into a joinable column
macro_clean = {
  macro %>% 
    select(Date, Population_Total_Mil_NSA,
           Income_Personal_Saving_Rate,
           Income_Per_Capita_Income,
           Household_Survey_Unemployment_Rate,
           Interest_Rates_10_Year,
           Interest_Rates_3_Month,
           S_P_500_Composite,
           Consumer_Credit_Total_Delinquencies,
           Charge_Off_Rates_All,
           Motor_gasoline_prices_Unleaded
           ) %>% 
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
           DateJOIN = lubridate::year(Date) * 100 + lubridate::month(Date),
           Delinquencies_Per_Capita = Consumer_Credit_Total_Delinquencies / Population_Total_Mil_NSA,
           ) %>% 
    select(-Consumer_Credit_Total_Delinquencies, -Population_Total_Mil_NSA)
}

# write into files
readr::write_csv(macro_clean, "data/macro-clean.csv")
