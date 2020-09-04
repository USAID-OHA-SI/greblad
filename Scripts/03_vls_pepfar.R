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

# MUNGE -------------------------------------------------------------------

  #filter to necessary variables
    df_vl <- df_mer %>% 
      filter(indicator %in% c("TX_CURR", "TX_PVLS"),
             standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"))
  
  #add D to PVLS denom
    df_vl <- df_vl %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator))
  
  #keep to just quarterly results to create VLC
    df_vl <- df_vl %>% 
      group_by(countryname, fundingagency, fiscal_year, indicator) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
      ungroup()
  
  #reshape long to setup VLC calc over quarters
    df_vl <- df_vl %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      spread(indicator, val)
  
  #calc prior TX_CURR for ease
    df_vl <- df_vl %>%
      group_by(countryname, fundingagency) %>%
      mutate(TX_CURR_2prior = lag(TX_CURR, 2, order_by = period)) %>% 
      ungroup()
  
  #calc VLC + VLS (due to Q3 issues in ZAF, need to use Q2 data)
    df_vl <- df_vl %>% 
      mutate(VLC = TX_PVLS_D/TX_CURR_2prior,
             VLS = (TX_PVLS/TX_PVLS_D)*VLC) %>% 
      filter(period == ifelse(countryname == "South Africa", "FY20Q2", max(period))) %>% 
      select(-period)
    
  #ensure every ctry has each agency (for USAID filtering) and then calc PEPFAR totals
    df_vl <- df_vl %>%
      complete(fundingagency, nesting(countryname)) %>% 
      group_by(countryname) %>% 
      mutate(VLC_pepfar = sum(TX_PVLS_D, na.rm = TRUE)/sum(TX_CURR_2prior, na.rm = TRUE),
             VLS_pepfar = (sum(TX_PVLS, na.rm = TRUE)/sum(TX_PVLS_D, na.rm = TRUE)*sum(VLC, na.rm = TRUE))) %>%
      ungroup() %>% 
      filter(fundingagency == "USAID") %>% 
      rename_with(~ paste0(., "_usaid"), c(VLC, VLS)) %>% 
      rename_all(tolower) %>% 
      select(countryname, starts_with("vls"))

  #add iso code in for merging
    df_vl <- df_vl %>% 
      mutate(iso = countrycode(countryname, "country.name", "iso3c", warn = FALSE)) %>% 
      relocate(iso, .after = "countryname")
    
# EXPORT ------------------------------------------------------------------
    
    write_csv(df_vl, "Dataout/pepfar_vls.csv", na = "")      
  
  