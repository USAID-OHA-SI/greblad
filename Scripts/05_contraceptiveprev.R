##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: munge contraceptive prevalance (WDI)
##  LICENCE: MIT
##  DATE:    2020-09-04
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(janitor)


# IMPORT ------------------------------------------------------------------

  #read
    df_con <- read_csv("Data/API_SP.DYN.CONU.ZS_DS2_en_csv_v2_1222781.csv", skip = 4)

# MUNGE -------------------------------------------------------------------
  
  #rename
    df_con <- df_con %>% 
      clean_names() %>% 
      select(countryname = country_name, 
             iso = country_code, 
             starts_with("x"))
  
           
  #reshape
    df_con <- df_con %>% 
      pivot_longer(starts_with("x"),
                   names_to = "contraceptive_prev_year",
                   names_prefix = "x",
                   values_to = "contraceptive_prev",
                   values_drop_na = TRUE)
    
  #covert to decimal
    df_con <- df_con %>% 
      mutate(contraceptive_prev = contraceptive_prev/100)
    
  #keep only latest observation
    df_con <- df_con %>% 
      group_by(iso) %>% 
      filter(contraceptive_prev_year == max(contraceptive_prev_year)) %>% 
      ungroup()
    
    
# EXPORT ------------------------------------------------------------------
    
    write_csv(df_con, "Dataout/contraceptive_prev.csv", na = "")   
      
      
      
      
  