##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: munge country capacity
##  LICENCE: MIT
##  DATE:    2020-09-03
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(glamr)
library(countrycode)
library(janitor)


# IMPORT ------------------------------------------------------------------

  df_capacity <- read_excel("Data/INFORM_GRI_2020_v040.xlsx",
                            sheet = "Lack of Coping Capacity",
                            skip = 1)


# MUNGE -------------------------------------------------------------------

  #make names useable
    df_capacity <- df_capacity %>%
      clean_names()
  
  #filter to key indicators
    df_capacity <- df_capacity %>% 
    select(countryname = country, iso = iso3, governance, hc_access = access_to_health_care_index) %>% 
    filter(!is.na(countryname)) 
    
  #calc capacity
    df_capacity <- df_capacity %>%  
      mutate(hc_access = as.double(hc_access)) %>% 
      pivot_longer(c(governance, hc_access), names_to = "index", values_to = "ctry_capacity") %>% 
      group_by(countryname, iso) %>% 
      summarise(ctry_capacity = mean(ctry_capacity)) %>% 
      ungroup() %>% 
      mutate(ctry_capacity = 10 - ctry_capacity)

# EXPORT ------------------------------------------------------------------

    write_csv(df_capacity, "Dataout/ctry_capacity.csv", na = "")
