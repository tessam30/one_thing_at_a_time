# PROJECT: one thing at a time
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  eb265fd7
# LICENSE: MIT
# DATE:   2023-07-26
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    
    
  # SI specific paths/functions  
    load_secrets()
    genie_path <- return_latest(folder = si_path(), pattern = "PSNU_IM.*Zambia")
      
  # Grab metadata
   get_metadata(genie_path)
  
  # REF ID for plots
    ref_id <- "eb265fd7"
    
  # Functions 
    # Functions  
    mech_names_order <- c("SAFE", "Action HIV", "DISCOVER-H", "ZAM Health") 
    
    
    sum_tx <- function(.data){ 
      .data %>% 
        summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
        ungroup() %>% 
        reshape_msd(direction = "quarters") %>% 
        mutate(achv = results_cumulative / targets,
               qtr_flag = ifelse(period %in% c(metadata$curr_pd), 1, 0),
               mech_name = fct_relevel(mech_name, mech_names_order))
    }
    
    plot_tx_trends <- function(.data, facet_var = mech_name, nrows = 1, scale_type = "fixed"){
      .data %>% 
        ggplot(aes(x = period)) +
        geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = -0.15), width = 0.75) +
        geom_col(aes(y = results_cumulative), fill = scooter, width = 0.75) +
        facet_wrap(enquo(facet_var), nrow = nrows, scales = scale_type) +
        si_style_ygrid(facet_space = 0.35) +
        scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                    "FY22Q1", "", "", "",
                                    "FY23Q1", "", "")) +
        scale_y_continuous(labels = comma, expand = c(0, 0)) +
        geom_text(data = . %>% filter(qtr_flag == 1 | period == "FY22Q4"), 
                  aes(y = results_cumulative, label = percent(achv, 1)),
                  family = "Source Sans Pro",
                  size = 11/.pt, 
                  vjust = -.5)
    }
  
    source("Scripts/helper-call_all_helpers.R")

# LOAD DATA ============================================================================  

    df_genie <- read_psd(genie_path) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets()

# MUNGE ============================================================================
  
    # TX_ML PEDS
    df_genie %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_ML", 
             standardizeddisaggregate == "Age/Sex/ARTCauseofDeath",
             ageasentered %in% c("01-04", "05-09", "10-14")) %>% 
      group_by(indicator, otherdisaggregate, fiscal_year) %>% 
      summarise(across(c(cumulative), \(x) sum(x, na.rm = T)), .groups = "drop") %>% 
      spread(fiscal_year, cumulative) %>% 
      janitor::adorn_totals(where = "row")
  
# TX_CURR PEDS ============================================================================
    
    tx_curr_all <- df_genie %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_CURR", 
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      group_by(mech_name, fiscal_year, indicator) %>% 
      sum_tx()
    
    tx_curr_snu <- df_genie %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_CURR", 
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      group_by(snu1, fiscal_year, indicator) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results_cumulative / targets,
             qtr_flag = ifelse(period %in% c(metadata$curr_pd), 1, 0),
             snu1_order = fct_reorder2(snu1, results, period, .desc = T)
      )
    
    # Copperbelt is hemmoraging tx_curr, where is this occuring specifically
    tx_curr_cbelt <- df_genie %>% 
      clean_column() %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_CURR", 
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             snu1 == "Copperbelt") %>% 
      group_by(psnu, fiscal_year, indicator) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results_cumulative / targets,
             qtr_flag = ifelse(period %in% c(metadata$curr_pd), 1, 0),
             psnu_order = fct_reorder2(psnu, targets, results, .desc = T)
      )
    
    tx_curr_cntral <- df_genie %>% 
      clean_column() %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_CURR", 
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             snu1 == "Central") %>% 
      group_by(psnu, fiscal_year, indicator) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results_cumulative / targets,
             qtr_flag = ifelse(period %in% c(metadata$curr_pd), 1, 0),
             psnu_order = fct_reorder2(psnu, targets, results, .desc = T)
      )
    
    
    tx_curr_peds <- df_genie %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_CURR", 
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             trendscoarse == "<15") %>% 
      group_by(mech_name, fiscal_year, indicator) %>% 
      sum_tx()
    
    tx_curr_ayp <- df_genie %>% 
      filter(funding_agency == "USAID", 
             indicator == "TX_CURR", 
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             ageasentered %in% c("15-19", "20-24")) %>% 
      group_by(mech_name, fiscal_year, indicator) %>% 
      sum_tx()
    

# VIZ ============================================================================

    # ALL
    tx_curr_all %>% 
      filter(mech_name %ni% c("CHEKUP II")) %>%
      plot_tx_trends() +
      labs(
        x = NULL, y = NULL, title = glue("TX_CURR TRENDS BY PARTNER AS OF {metadata$curr_pd}"),
        subtitle = "Gray bars are TX_CURR targets",
        caption = metadata$caption
      )
    si_save(glue("Images/Adhoc/{metadata$curr_pd}_TX_CURR_trends.png"), scale = 1.25)
    
    # PEDS
    tx_curr_peds %>% 
      filter(mech_name %ni% c("CHEKUP II")) %>%
      plot_tx_trends() +
      labs(
        x = NULL, y = NULL, title = glue("TX_CURR PEDIATRIC TRENDS BY PARTNER AS OF {metadata$curr_pd}"),
        subtitle = "Gray bars are TX_CURR targets",
        caption = metadata$caption
      )
    si_save(glue("Images/Adhoc/{metadata$curr_pd}_TX_CURR_pediatric_trends.png"), scale = 1.25)
    
    # AYP
    tx_curr_ayp %>%
      filter(mech_name %ni% c("CHEKUP II")) %>%
      plot_tx_trends() +
      labs(
        x = NULL, y = NULL, title = glue("TX_CURR AYP (15-24) TRENDS BY PARTNER AS OF {metadata$curr_pd}"),
        subtitle = "Gray bars are TX_CURR targets",
        caption = metadata$caption
      )
    si_save(glue("Images/{metadata$curr_pd}_TX_CURR_AYP_trends.png"), scale = 1.25)
    
 # BY SNU1 & PSNU OVERALL ============================================================================
    
    tx_curr_snu %>%
      filter(snu1 %ni% c("Southern", "Eastern", "Lusaka")) %>%
      mutate(snu1 = fct_reorder2(snu1, targets, results, .desc = T)) %>%
      group_by(snu1) %>%
      mutate(
        tx_curr_trend = case_when(
          results < lag(results) ~ golden_sand,
          TRUE ~ NA_character_
        ),
        tx_curr_diff = results - lag(results)
      ) %>%
      ungroup() %>%
      complete(snu1, period) %>% 
      plot_tx_trends(., facet_var = snu1, nrows = 2, scale_type = "free") +
      geom_col(
        data = . %>% filter(period == max(period)),
        aes(y = results, fill = tx_curr_trend), width = 0.75
      ) +
      scale_fill_identity() +
      geom_label(
        data = . %>% filter(period == max(period)),
        aes(y = results, label = comma(tx_curr_diff)),
        vjust = 1.2,
        family = "Source Sans Pro",
        size = 10 / .pt,
        color = grey90k
      ) +
      labs(
        x = NULL, y = NULL, title = glue("TX_CURR TRENDS BY PROVINCE AS OF {metadata$curr_pd}"),
        subtitle = "Gray bars are TX_CURR targets",
        caption = metadata$caption
      ) +
      coord_cartesian(clip = "off")
    si_save(glue("Images/Adhoc/{metadata$curr_pd}_TX_CURR_SNU1_trends.png"), scale = 1.25)
    
    # COPPERBELT
    tx_curr_cbelt %>% 
      group_by(psnu) %>% 
      mutate(tx_curr_trend = case_when(
        results < lag(results) ~ golden_sand, 
        TRUE ~ NA_character_
      ),
      tx_curr_diff = results - lag(results)) %>% 
      ungroup() %>% 
      plot_tx_trends(., facet_var = psnu_order, nrows = 3, scale_type = "free") +
      geom_col(data = . %>% filter(period == max(period)), 
               aes(y = results, fill = tx_curr_trend), width = 0.75) +
      scale_fill_identity() +
      geom_text(data = . %>% filter(period == max(period)),
                aes(y = results, label = comma(tx_curr_diff)), 
                vjust = 1,
                family = "Source Sans Pro",
                size = 11/.pt, 
                color = grey90k) +
      labs(x = NULL, y = NULL, title = glue("TX_CURR TRENDS BY COPPERBELT DISTRICTS AS OF {metadata$curr_pd}"),
           subtitle = "Gray bars are TX_CURR targets",
           caption = metadata$caption) +
      coord_cartesian(clip = "off")
    si_save(glue("Images/Adhoc/{metadata$curr_pd}_TX_CURR_PSNU_trends.png"), scale = 1.25)
    
    # CENTRAL
    tx_curr_cntral %>% 
      group_by(psnu) %>% 
      mutate(tx_curr_trend = case_when(
        results < lag(results) ~ golden_sand, 
        TRUE ~ NA_character_
      ),
      tx_curr_diff = results - lag(results)) %>% 
      ungroup() %>% 
      plot_tx_trends(., facet_var = psnu_order, nrows = 3, scale_type = "free") +
      geom_col(data = . %>% filter(period == max(period)), 
               aes(y = results, fill = tx_curr_trend), width = 0.75) +
      scale_fill_identity() +
      geom_text(data = . %>% filter(period == max(period)),
                aes(y = results, label = comma(tx_curr_diff)), 
                vjust = 1,
                family = "Source Sans Pro",
                size = 11/.pt, 
                color = grey90k) +
      labs(x = NULL, y = NULL, title = glue("TX_CURR TRENDS BY CENTRAL DISTRICTS AS OF {metadata$curr_pd}"),
           subtitle = "Gray bars are TX_CURR targets",
           caption = metadata$caption) +
      coord_cartesian(clip = "off")
    si_save(glue("Images/Adhoc/{metadata$curr_pd}_TX_CURR_CENTRAL_PSNU_trends.png"), scale = 1.25)
    
