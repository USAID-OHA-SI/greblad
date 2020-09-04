##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: munge immunization coverage
##  LICENCE: MIT
##  DATE:    2020-09-04
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(curl)
library(httr)
library(jsonlite)
library(countrycode)


# IMPORT ------------------------------------------------------------------

  #API url
    url <- "https://api.dhsprogram.com/rest/dhs/data/CH_VACS_C_BAS?f=json"
  
  #access and store as tibble
    df_vacs <- url %>%
      httr::GET() %>%
      httr::content("text") %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("Data") %>%
      tibble::as_tibble()


# MUNGE -------------------------------------------------------------------

  #keep only lastest by country
    df_vacs <- df_vacs %>% 
      group_by(CountryName) %>% 
      filter(SurveyYear == max(SurveyYear)) %>% 
      ungroup() 
    
  #filter to only necessary vars
    df_vacs <- df_vacs %>% 
      select(countryname = CountryName, vacs_coverage = Value, vacs_coverage_year = SurveyYear)

  #covert to percent
    df_vacs <- df_vacs %>% 
      mutate(vacs_coverage = vacs_coverage/100)
    
  #add iso code
    df_vacs <- df_vacs %>% 
      mutate(iso = countrycode(countryname, "country.name", "iso3c", warn = FALSE)) %>% 
      relocate(iso, .after = "countryname")
    
# EXPORT ------------------------------------------------------------------
    
    write_csv(df_vacs, "Dataout/immunizations.csv", na = "")   
    
    