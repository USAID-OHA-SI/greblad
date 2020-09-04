##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: munge budget data
##  LICENCE: MIT
##  DATE:    2020-09-03
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(googlesheets4)
  library(janitor)
  library(countrycode)


# IMPORT ------------------------------------------------------------------

  #import from G Drive
  sheet_id <- "15REkQ2kNIgYR3LQ8k7WnQYmXBY4UmZpaRNwRQqI-V7I"
  gs4_auth()
  df_copmatrix <- read_sheet(sheet_id)


# MUNGE -------------------------------------------------------------------

  #make names usable  
    df_copmatrix <- clean_names(df_copmatrix)

  #create planning lvl w/o M&O and w/ + w/o supply chain; aggregate to OU x agency level
    df_copmatrix_agg <- df_copmatrix %>%
      filter(record_type != "Management and Operations") %>% 
      mutate(total_planned_funding_sans_ghsc = case_when(str_detect(mechanism_name, "GHSC", negate = TRUE) ~ total_planned_funding)) %>% 
      group_by(operating_unit, funding_agency) %>% 
      summarise(cop20_budget = sum(total_planned_funding, na.rm = TRUE),
                cop20_budget_sans_ghsc = sum(total_planned_funding_sans_ghsc, na.rm = TRUE)) %>% 
      ungroup()

  #USAID share of PEPFAR budget by OU
    df_budget <- df_copmatrix_agg %>% 
      group_by(operating_unit) %>% 
      mutate(cop20_share = cop20_budget/sum(cop20_budget, na.rm = TRUE),
             cop20_share_sans_ghsc = cop20_budget_sans_ghsc/sum(cop20_budget_sans_ghsc, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(funding_agency == "USAID") %>% 
      rename_all(~str_replace(., "cop20", "cop20_usaid")) %>% 
      rename(operatingunit = operating_unit) %>% 
      select(-funding_agency)
    
  #identify ISO
    df_budget <- df_budget %>% 
      mutate(iso = countrycode(operatingunit, "country.name", "iso3c", warn = FALSE))
    
  #remove regional budgets not assigned to any OU
    df_budget <- df_budget %>% 
      filter(str_detect(operatingunit, "Region", negate = TRUE)) %>% 
      rename(countryname = operatingunit)

# EXPORT ------------------------------------------------------------------

  write_csv(df_budget, "Dataout/budgetshare.csv", na = "")    

