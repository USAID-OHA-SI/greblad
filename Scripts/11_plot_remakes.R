##  PROJECT: greblad
##  AUTHOR:  achafetz | USAID
##  PURPOSE: recreate graphics with country level data
##  LICENCE: MIT
##  DATE:    2020-09-04
##  UPDATE:  2020-09-08


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(glitr)
library(glamr)
library(extrafont)
library(scales)
library(ggrepel)
library(patchwork)

# IMPORT ------------------------------------------------------------------

  #import outputs from earlier scripts
    df_budget <- read_csv("Dataout/budgetshare.csv")
    df_pepfar_vls <- read_csv("Dataout/pepfar_vls.csv")
    df_capacity <- read_csv("Dataout/ctry_capacity.csv") %>% select(-countryname)
    df_unaids <- read_csv("Dataout/plhiv_vls.csv") %>% select(-countryname)
    df_phia <- read_csv("Data/phia_vls.csv") %>% select(-countryname)
    df_con_prev <- read_csv("Dataout/contraceptive_prev.csv") %>% select(-countryname)
    df_immunizations <- read_csv("Dataout/immunizations.csv") %>% select(-countryname)

# MUNGE -------------------------------------------------------------------

  #join pepfar data   
    df_pepfar <- full_join(df_budget, df_pepfar_vls)

  #left join other data by iso
    df_full <- df_pepfar %>% 
      left_join(df_capacity) %>% 
      left_join(df_unaids) %>% 
      left_join(df_phia) %>% 
      left_join(df_con_prev) %>% 
      left_join(df_immunizations)

  #add continent
    df_full <- df_full %>% 
      mutate(region = countrycode(iso, "iso3c", "continent")) %>% 
      relocate(iso, region, .after = countryname)

    
  #include PHIA numbers for VL where missing from UNAIDS
    df_full <- df_full %>% 
      mutate(vls_combo = ifelse(!is.na(vls), vls, vls_phia),
             vls_combo_source = case_when(countryname == "Nigeria" ~ "NAIIS",
                                          !is.na(vls) ~ "UNAIDS",
                                          !is.na(vls_phia) ~ "PHIA"))
      
    
  glimpse(df_full)

# CHECK -------------------------------------------------------------------

  orig <- c("Angola", "Botswana", "Burundi", "Cameroon", 
            "Cote d'Ivoire", "Democratic Republic of the Congo", 
            "Dominican Republic", "Eswatini", "Ethiopia", "Haiti", 
            "Kenya", "Lesotho", "Malawi", "Mozambique", "Namibia", 
            "Nigeria", "Rwanda", "South Africa", "South Sudan", "Tanzania", 
            "Uganda", "Vietnam", "Zambia", "Zimbabwe") %>% 
  countrycode("country.name", "iso3c")

  df_full <- df_full %>% 
    mutate(orig = iso %in% orig)

# PLOT --------------------------------------------------------------------

  #compare PEPFAR VLS proxy to UNAIDS
  
  df_full %>% 
    filter(is.na(vls)) %>% 
    arrange(plhiv_pepfar) %>% 
    pull(countryname) %>% 
    paste0(collapse = ", ")
  
    vl1 <- df_full %>%
      filter(!is.na(vls)) %>% 
      mutate(close = ifelse((vls_pepfar >= vls_lower & vls_pepfar <= vls_upper),
                            "Close", "Not Close"),
             close = ifelse(is.na(close), "Not Close", close)) %>% 
      ggplot(aes(y = fct_reorder(countryname, plhiv))) +
      geom_vline(xintercept = (.95 * .95 * .95), linetype = "dashed") +
      geom_errorbar(aes(xmin = vls_lower, xmax = vls_upper), width = 0, color = "gray80") + 
      geom_point(aes(x = vls), size = 3, color = "gray80") +
      geom_point(aes(x = vls_pepfar), size = 4, color = USAID_medblue) +
      facet_grid(close~., scales = "free_y", space = "free_y") +
      labs(x = "VLS", y = NULL
           #subtitle = "FY19Q4 TX_PVLS_N / FY19 PLHIV (Data Pack)"
           ) +
      scale_x_continuous(labels = percent) +
      si_style_xgrid() +
      theme(strip.text = element_blank(),
            # axis.text.y = element_blank(size = 8),
            panel.spacing.y=unit(1, "lines"))
    
    vl1h <- df_full %>% 
      filter(!is.na(vls)) %>%
      mutate(close = ifelse((vls_pepfar >= vls_lower & vls_pepfar <= vls_upper),
                            "Close", "Not Close"),
             close = ifelse(is.na(close), "Not Close", close)) %>% 
      mutate(variance = abs(vls - vls_pepfar)) %>% 
      select(countryname, variance) %>% 
      slice_max(variance, n = 8)
      ggplot(aes(x = variance, y = fct_reorder(countryname, plhiv))) +
      geom_col(fill = "gray50") +
      geom_vline(xintercept = 0) +
      facet_grid(close~., scales = "free_y", space = "free_y") +
      scale_x_continuous(expand = c(.0005, .0005)) +
      expand_limits(x = .75) +
      labs(x = NULL, y = NULL) +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.spacing.y=unit(1, "lines"))
    vl1 + vl1h + 
      plot_layout(width = c(6, 1)) +
      plot_annotation(
        title = 'PEPFAR DATA NOT A CLOSE SUBSTITUTE',
        subtitle = 'Most VLS program data fall outside the UNAIDS VLS estimate bounds to justify using where no UNAIDS data exists',
        caption = 'PEPFAR VLS = FY19Q4 TX_PVLS_N / FY19 PLHIV
        UNAIDS CY19 People living with HIV who have suppressed viral loads (%)
        Sources: PEPFAR MSD + IMPATT FY20Q3i, UNAIDS [2020-09-04]',
        theme = si_style()
      )
    
    
    ggsave("Images/VLS_comparison.pdf", device = cairo_pdf,
           height = 7, width = 8, units = "in")
    
    
    vl2 <- df_full %>%
      filter(!is.na(vls)) %>% 
      ggplot(aes(y = fct_reorder(countryname, plhiv))) +
      geom_vline(xintercept = (.95 * .95 * .95), linetype = "dashed") +
      geom_errorbar(aes(xmin = vls_lower, xmax = vls_upper), width = 0, color = "gray80") + 
      geom_point(aes(x = vls), size = 3, color = "gray80") +
      geom_point(aes(x = vls_proxy_pepfar), size = 4, color = USAID_ltblue) +
      labs(x = NULL, y = NULL, subtitle = "FY20Q3 TX_PVLS_N / FY20Q1 TX_CURR") +
      scale_x_continuous(labels = percent) +
      si_style_xgrid()
    
    vl2h <- df_full %>% 
      filter(!is.na(vls)) %>% 
      mutate(variance = abs(vls - vls_proxy_pepfar)) %>% 
      ggplot(aes(x = variance, y = fct_reorder(countryname, plhiv))) +
      geom_col(fill = "gray50") +
      geom_vline(xintercept = 0) +
      expand_limits(x = .75) +
      scale_x_continuous(expand = c(.0005, .0005)) +
      labs(x = NULL, y = NULL) +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    vl1 + vl1h + vl2 + vl2h + 
      plot_layout(width = c(5, 1, 5, 1))
    

    
    ggsave("Images/VLS_comparison.pdf", device = cairo_pdf,
           height = 4, width = 8, units = "in")

  #VLS v country capacity
    df_full %>% 
      select(region, countryname, orig, vls, ctry_capacity) %>% 
      arrange(region, orig) %>% 
      prinf()
  
    df_full %>% 
      ggplot(aes(vls, ctry_capacity)) +
      geom_vline(xintercept = (.95 * .95 * .95), linetype = "dashed", color = "gray30") +
      # geom_hline(yintercept = mean(df_full$ctry_capacity)) +
      # geom_point(aes(color = region == "Asia"), size = 4, show.legend = FALSE, na.rm = TRUE) +
      geom_text_repel(aes(label = countryname), na.rm = TRUE,
                      family = "Source Sans Pro", size = 4, color = "gray50") +
      geom_point(size = 4, alpha = .6, na.rm = TRUE) +
      scale_x_continuous(label = percent) +
      # scale_color_manual(values = c(USAID_lgrey, USAID_medblue)) +
      si_style()
    
  #VLS v budget share
    df_full %>% 
      ggplot(aes(vls, cop20_usaid_share)) +
      geom_vline(xintercept = (.95 * .95 * .95), linetype = "dashed", color = "gray30") +
      geom_hline(yintercept = .5, linetype = "dashed", color = "gray30") +
      geom_text_repel(aes(label = iso), na.rm = TRUE,
                      family = "Source Sans Pro", size = 4, color = "gray50") +
      geom_point(aes(size = cop20_usaid_budget), color = USAID_medblue, alpha = .6, na.rm = TRUE) +
      expand_limits(y = 0, x = 0)  +      
      scale_x_continuous(label = percent) +
      scale_y_continuous(label = percent) +
      scale_size(range = c(1, 14), labels = comma) +
      si_style()

  #VLS v budget share x non-HIV budget share
    
  #VLS v Contraception
    v1 <- df_full %>% 
      ggplot(aes(vls, contraceptive_prev)) +
      geom_vline(xintercept = (.95 * .95 * .95), linetype = "dashed", color = "gray30") +
      geom_text_repel(aes(label = countryname), na.rm = TRUE,
                      family = "Source Sans Pro", size = 4, color = "gray50") +
      geom_point(size = 4, alpha = .6, na.rm = TRUE) +
      scale_x_continuous(label = percent) +
      scale_y_continuous(label = percent) +
      expand_limits(x = c(0, 1), y = c(0, 1)) +
      si_style()
    
  #VLS v Immunization
    v2 <- df_full %>% 
      ggplot(aes(vls, vacs_coverage)) +
      geom_vline(xintercept = (.95 * .95 * .95), linetype = "dashed", color = "gray30") +
      geom_text_repel(aes(label = countryname), na.rm = TRUE,
                      family = "Source Sans Pro", size = 4, color = "gray50") +
      geom_point(size = 4, alpha = .6, na.rm = TRUE) +
      scale_x_continuous(label = percent) +
      scale_y_continuous(label = percent) +
      expand_limits(x = c(0, 1), y = c(0, 1)) +
      si_style()

    v1 + v2    
    