##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: MER VLS
##  LICENCE: MIT
##  DATE:    2020-09-03
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ICPIutilities)


# IMPORT ------------------------------------------------------------------
  
  #local folder path to the OUxIM MSD
    msd_fldr <- "~/Data/"
  
  #read
    df_mer <- list.files(msd_fldr, "OU_IM", full.names = TRUE) %>% 
      read_rds()
    
    df_nat <- list.files(msd_fldr, "NAT_SUBNAT", full.names = TRUE) %>% 
      read_rds()

# MUNGE -------------------------------------------------------------------

  #filter to PLHIV
    df_nat <- df_nat %>% 
      filter(indicator == "PLHIV",
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(countryname, fiscal_year) %>% 
      summarise(plhiv = sum(targets, na.rm = TRUE)) %>% 
      ungroup()
  
  #keep the closest year to 2019
    df_nat <- df_nat %>% 
      mutate(pref_year = case_when(fiscal_year == 2019 ~ 1, 
                                   fiscal_year == 2020 ~ 2,
                                   fiscal_year == 2018 ~ 3,
                                   fiscal_year == 2021 ~ 4,
                                   fiscal_year == 2017 ~ 5,
                                   fiscal_year == 2016 ~ 6)) %>% 
      group_by(countryname) %>% 
      filter(pref_year == min(pref_year)) %>% 
      ungroup() %>% 
      select(-pref_year) %>% 
      rename(vls_pepfar_plhiv_year = fiscal_year)
    
  #filter to necessary variables
    df_vl <- df_mer %>% 
      filter(indicator == "TX_PVLS",
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2019)
  
  #summarize to FY19Q4
    df_vl <- df_vl %>% 
      group_by(countryname) %>% 
      summarise(tx_pvls = sum(qtr4, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(tx_pvls_year = "FY19Q4")
  
  #calc VLS - PVLS_N FY19Q4
    df_vls <- df_vl %>% 
      full_join(df_nat) %>%
      mutate(vls_pepfar = tx_pvls / plhiv)
      

  #add iso code in for merging
    df_vls <- df_vls %>% 
      mutate(iso = countrycode(countryname, "country.name", "iso3c", warn = FALSE)) %>% 
      relocate(iso, .after = "countryname")
    
# EXPORT ------------------------------------------------------------------
    
    write_csv(df_vl, "Dataout/pepfar_vls.csv", na = "")      
  
  