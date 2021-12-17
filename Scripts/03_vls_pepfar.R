##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: MER VLS
##  LICENCE: MIT
##  DATE:    2020-09-03
##  UPDATE:  2020-09-08


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(countrycode)
library(gophr)


# IMPORT ------------------------------------------------------------------
  
  #local folder path to the OUxIM MSD
    msd_fldr <- "~/Data/"
    msd_fldr <- si_path()
  
  #read
    df_mer <- return_latest(msd_fldr, "OU_IM_FY19-22") %>% read_msd()
    
    df_nat <- return_latest(msd_fldr, "NAT_SUBNAT") %>% read_msd()

# MUNGE -------------------------------------------------------------------

  #filter to PLHIV
    df_nat <- df_nat %>% 
      filter(indicator == "PLHIV",
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(countryname, fiscal_year) %>% 
      summarise(plhiv_pepfar = sum(targets, na.rm = TRUE), .groups = "drop") 
  
  #keep the closest year to 2019
    df_nat %>% distinct(fiscal_year) %>% arrange(fiscal_year)
  #2016 2017 2018 2019 2020 2021 2022
  #2019, 2020, 2018, 2021, 2017, 2016
    df_nat %>% filter(fiscal_year == 2020)
    
    # df_nat <- df_nat %>% 
    #   mutate(pref_year = case_when(fiscal_year == 2019 ~ 1, 
    #                                fiscal_year == 2020 ~ 2,
    #                                fiscal_year == 2018 ~ 3,
    #                                fiscal_year == 2021 ~ 4,
    #                                fiscal_year == 2017 ~ 5,
    #                                fiscal_year == 2016 ~ 6))
    
    df_nat <- df_nat %>% 
      mutate(pref_year = case_when(fiscal_year == 2020 ~ 1, 
                                   fiscal_year == 2021 ~ 2,
                                   fiscal_year == 2019 ~ 3,
                                   fiscal_year == 2022 ~ 4,
                                   fiscal_year == 2018 ~ 5,
                                   fiscal_year == 2017 ~ 6,
                                   fiscal_year == 2016 ~ 7)) %>% 
      group_by(countryname) %>% 
      filter(pref_year == min(pref_year)) %>% 
      ungroup() %>% 
      select(-pref_year) %>% 
      rename(plhiv_pepfar_year = fiscal_year)
    
  #filter to necessary variables
    df_vl <- df_mer %>% 
      filter(indicator == "TX_PVLS",
             standardizeddisaggregate  == "Total Numerator",
             fiscal_year == 2020)
    
  #summarize to FY20Q4
    df_vl <- df_vl %>% 
      group_by(countryname) %>% 
      summarise(tx_pvls = sum(qtr4, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(tx_pvls_year = "FY20Q4")
    
  #calc VLS - PVLS_N FY20Q4
    df_vls <- df_vl %>% 
      full_join(df_nat) %>%
      mutate(vls_pepfar = tx_pvls / plhiv_pepfar)
    
    
  #filter to necessary variables
    df_vl_proxy <- df_mer %>% 
      filter(indicator %in% c("TX_CURR", "TX_PVLS"),
             standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"))
  
  #add D to PVLS denom
    df_vl_proxy <- df_vl_proxy %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator))
    
  #keep to just quarterly results to create VLC
    df_vl_proxy <- df_vl_proxy %>% 
      group_by(countryname, fundingagency, fiscal_year, indicator) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") 
    
  #reshape long to setup VLC calc over quarters
    df_vl_proxy <- df_vl_proxy %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      spread(indicator, value)
    
  #calc prior TX_CURR for ease
    df_vl_proxy <- df_vl_proxy %>%
      group_by(countryname, fundingagency) %>%
      mutate(TX_CURR_2prior = lag(TX_CURR, 2, order_by = period)) %>% 
      ungroup()
    
  #calc VLC + VLS (due to Q3 issues in ZAF, need to use Q2 data)
    df_vl_proxy <- df_vl_proxy %>% 
      mutate(VLC = TX_PVLS_D/TX_CURR_2prior,
             VLS_proxy = (TX_PVLS/TX_PVLS_D)*VLC) %>% 
      #filter(period == ifelse(countryname == "South Africa", "FY20Q2", max(period))) %>%       
      filter(period == ifelse(countryname == "South Africa", "FY20Q2", "FY20Q4")) %>% 
      select(-period)
    
  #ensure every ctry has each agency (for USAID filtering) and then calc PEPFAR totals
    df_vl_proxy <- df_vl_proxy %>%
      complete(fundingagency, nesting(countryname)) %>% 
      group_by(countryname) %>% 
      mutate(VLC_pepfar = sum(TX_PVLS_D, na.rm = TRUE)/sum(TX_CURR_2prior, na.rm = TRUE),
             VLS_proxy_pepfar = (sum(TX_PVLS, na.rm = TRUE)/sum(TX_PVLS_D, na.rm = TRUE))*VLC) %>%
      ungroup() %>% 
      filter(fundingagency == "USAID") %>% 
      rename_with(~ paste0(., "_usaid"), c(VLC, VLS_proxy)) %>% 
      rename_all(tolower) %>% 
      select(countryname, starts_with("vls"))
    

  #merge, adding iso code
    df_vls <- df_vls %>%
      full_join(df_vl_proxy) %>% 
      mutate(iso = countrycode(countryname, "country.name", "iso3c", warn = FALSE)) %>% 
      relocate(iso, .after = "countryname")
    
# EXPORT ------------------------------------------------------------------
    
    write_csv(df_vls, "Dataout/pepfar_vls.csv", na = "")      
  
  