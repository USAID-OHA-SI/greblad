##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: munge UNAIDS PLHIV and VLS data
##  LICENCE: MIT
##  DATE:    2020-09-03
##  UPDATE:  2021-07-20


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glamr)
  library(countrycode)


# GLOBAL VARIABLES --------------------------------------------------------

  curr_year <- "2020"

# IMPORT ------------------------------------------------------------------

  #import
    df_unaids <- map2_dfr(.x = c("Data/People living with HIV_People living with HIV - All ages_Population_ All.csv",
                    "Data/Treatment cascade_People living with HIV who have suppressed viral loads (%)_Population_ All ages.csv"),
             .y = c("PLHIV", "VLS"),
             .f = ~ read_csv(.x) %>% mutate(indicator = .y)
             )

# MUNGE -------------------------------------------------------------------

  #add iso code for merging with PEPFAR data
    df_unaids <- df_unaids %>% 
      mutate(iso = countrycode(Country, "country.name", "iso3c", warn = FALSE))

  #cleanup names
    df_unaids <- df_unaids %>% 
      select(countryname = Country, iso, indicator, starts_with(curr_year)) %>% 
      rename_all(~str_replace(., curr_year, "value")) %>% 
      mutate(across(starts_with("value"), 
                    ~ str_remove_all(., "<|>| ")) %>% na_if(., "...")) %>% 
      mutate(across(starts_with("value"), as.double))
    
  #remove blank rows
    df_unaids <- df_unaids %>% 
      filter(!is.na(value))
    
  #reshape
    df_unaids <- df_unaids %>% 
      pivot_wider(names_from = indicator, 
                  names_glue = "{indicator}_{.value}",
                  values_from = c(value, value_lower, value_upper)) %>% 
      rename_all(~str_remove(., "_value") %>% tolower) %>% 
      select(countryname, iso, starts_with("plhiv"), starts_with("vls")) %>%
      mutate(across(starts_with("vls"), ~ ./100))

# EXPORT ------------------------------------------------------------------
    
    write_csv(df_unaids, "Dataout/plhiv_vls.csv", na = "")    
    
