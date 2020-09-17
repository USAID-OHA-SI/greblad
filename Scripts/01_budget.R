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
  df_copmatrix19 <- read_sheet(sheet_id, sheet = "2019 Cop matrix")
  
  sheet_id_gh <- "1rzlWJgVW1fpQ3CLS26fwnDcq4PKsVS-uqEZBk67Ojy8"
  df_ghbudget <- read_sheet(sheet_id_gh)


# MUNGE -------------------------------------------------------------------

  #make names usable  
    df_copmatrix <- df_copmatrix %>% 
      bind_rows(df_copmatrix19) %>% 
      clean_names()

  #create planning lvl w/o M&O and w/ + w/o supply chain; aggregate to OU x agency level
    df_copmatrix_agg <- df_copmatrix %>%
      filter(record_type != "Management and Operations") %>% 
      mutate(total_planned_funding_sans_ghsc = case_when(str_detect(mechanism_name, "GHSC", negate = TRUE) ~ total_planned_funding),
             planning_cycle = str_remove_all(planning_cycle, "^20| COP") %>% paste0("cop", .)) %>% 
      group_by(operating_unit, funding_agency, planning_cycle) %>% 
      summarise(budget = sum(total_planned_funding, na.rm = TRUE),
                budget_sans_ghsc = sum(total_planned_funding_sans_ghsc, na.rm = TRUE)) %>% 
      ungroup()

  #USAID share of PEPFAR budget by OU
    df_budget <- df_copmatrix_agg %>% 
      group_by(operating_unit) %>% 
      mutate(share = budget/sum(budget, na.rm = TRUE),
             share_sans_ghsc = budget_sans_ghsc/sum(budget_sans_ghsc, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(funding_agency == "USAID") %>% 
      pivot_wider(names_from = planning_cycle, 
                  names_glue = "{planning_cycle}_{.value}",
                  values_from = c(budget, budget_sans_ghsc, share, share_sans_ghsc)) %>% 
      rename_all(~str_replace(., "cop(19|20)", "cop\\1_usaid")) %>% 
      rename(operatingunit = operating_unit) %>% 
      select(-funding_agency)
    
  #identify ISO
    df_budget <- df_budget %>% 
      mutate(iso = countrycode(operatingunit, "country.name", "iso3c", warn = FALSE))
    
  #remove regional budgets not assigned to any OU
    df_budget <- df_budget %>% 
      filter(str_detect(operatingunit, "Region", negate = TRUE)) %>% 
      rename(countryname = operatingunit)
    
    
## GH Budget
  #make names usable
    df_ghbudget <- clean_names(df_ghbudget)
    
    df_ghbudget <- df_ghbudget %>% 
      select(countryname = bureau_program, ends_with("total_allocation"), hiv_aids) %>% 
      rowwise() %>% 
      mutate(fy20_usaid_ghbudget = sum(ghp_state_fy_2020_total_allocation, 
                                       ghp_usaid_fy_2020_total_allocation, na.rm = TRUE) - sum(hiv_aids, na.rm = TRUE)) %>% 
      ungroup() %>% 
      select(countryname, fy20_usaid_ghbudget) %>% 
      mutate(fy20_usaid_ghbudget = fy20_usaid_ghbudget * 1000)
    
#merge
    df_budget <- df_budget %>% 
      left_join(df_ghbudget)
    
    
    df_budget <- df_budget %>% 
      relocate(iso, .after = "countryname")
    
    # df_budget %>% 
    #   select(countryname, cop19_usaid_budget, fy20_usaid_ghbudget) %>%
    #   mutate(countryname = fct_reorder(countryname, fy20_usaid_ghbudget)) %>% 
    #   ggplot(aes(y = countryname)) +
    #   geom_col(aes(x = fy20_usaid_ghbudget/1000000), fill = "gray") +
    #   geom_col(aes(x = cop19_usaid_budget/1000000), fill = "black") +
    #   scale_x_continuous(label = scales::comma) +
    #   labs(x = "FY20 Budget (millions USD)", y = NULL) 
    #   prinf()

# EXPORT ------------------------------------------------------------------

  write_csv(df_budget, "Dataout/budgetshare.csv", na = "")    

