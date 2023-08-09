# PROJECT: one thing at a time
# PURPOSE: Generate sortable site level google doc of high IIT sites
# AUTHOR:  Tim Esssam | SI
# REF ID:  1c0daab0
# LICENSE: MIT
# DATE:   2023-08-07
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
    site_path <- "Data/Genie-SiteByIMs-Zambia-Daily-2023-07-26.zip"
    
  # Excel file with smartcare sites flagged
    sc_data <- "Data/CBP smartcare plus facility list.xlsx"
    
  # Grab metadata
    get_metadata(site_path)
  
  # REF ID for plots
    ref_id <- "1c0daab0"
    
  # Functions  
    source("Scripts/helper-call_all_helpers.R")

# LOAD DATA ============================================================================  

    df_site <- read_psd(site_path) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets()
    
    df_sc <- readxl::read_excel(sc_data) %>% 
      janitor::clean_names()

# MUNGE ============================================================================
  
  # Identify sites with high IIT
    df_iit_comp <- df_site %>% 
      filter(funding_agency == "USAID", 
             indicator %in% c("TX_ML", "TX_CURR"), 
             standardizeddisaggregate %in% c("Total Numerator")) %>% 
      group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      filter(facility != "Data reported above Facility level") %>% 
      mutate(has_iit = !is.na(tx_ml),
             has_txcurr = !is.na(tx_curr))
    
   # IIT report by FY / SNU1
    df_iit_comp %>% 
      group_by(period, snu1) %>% 
      summarise(iit_comp = sum(has_iit)/sum(has_txcurr)) %>% 
      pivot_wider(names_from = period, values_from = iit_comp)
    
  # Calculate IIT by site for export
    df_iit <- df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
      group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator, mech_code, mech_name_short) %>% 
      #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      arrange(period) %>% 
      group_by(trendscoarse, facilityuid) %>% 
      mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
      ungroup() %>% 
      rowwise() %>% 
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
      ungroup()
    
    
  # Flag OVC SERV Sites
    # FLAG OVC SERV SITES
    df_ovc_site <- 
      df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("OVC_SERV"), 
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, snu1, facility, facilityuid, indicator, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      group_by(facility) %>% 
      mutate(ovc_facility = ifelse(ovc_serv>0, 1, NA_integer_),
             fy = substr(period, 3, 4)) %>% 
      group_by(facility, fy) %>% 
      fill(ovc_facility, .direction = "updown") %>% 
      ungroup() %>% 
      filter(facility != "Data reported above Facility level") %>% 
      select(period, facilityuid, ovc_facility, ovc_serv) %>% 
      filter(str_detect(period, metadata$curr_fy_lab))
    
    # FOLDING IN OTHER INDICATORS THAT WERE REQUESTED
    # HTS_TST_POS and VLC
    df_site_hts <- 
      df_site %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_PVLS", "TX_CURR"),
             standardizeddisaggregate %in% c("Age/Sex/Indication/HIVStatus", "Age/Sex/HIVStatus", "Modality/Age/Sex/Result")) %>% 
      clean_indicator() %>% 
      filter(indicator != "TX_PVLS", facility != "Data reported above Facility level") %>%
      group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator, mech_code) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      group_by(facilityuid, trendscoarse, mech_code) %>% 
      mutate(tx_curr_lag2 = lag(tx_curr, n = 2, order_by = period)) %>% 
      mutate(vlc = tx_pvls_d / tx_curr_lag2, .after = tx_pvls_d) %>% 
      mutate(positivity = hts_tst_pos / hts_tst, .after = hts_tst_pos) 
    
  # Combine em all
    df_iit_extract <- df_iit %>% 
      filter(str_detect(period, metadata$curr_fy_lab),
             trendscoarse != "Unknown Age") %>% 
      group_by(facility, snu1, mech_code, trendscoarse) %>% 
      mutate(iit_flag = ifelse(iit > .025, 1, 0),
             tot_iit = sum(iit_flag, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(iit = ifelse(is.infinite(iit), NA_real_, iit))
      filter(tot_iit >= 2) %>% 
      left_join(., df_site_hts) %>% 
      left_join(., df_ovc_site) %>% 
      left_join(., df_sc, by = c("facilityuid" = "orgunituid"))
    
   googlesheets4::write_sheet(data = df_iit_extract, ss = "1EHUknVo6W2Ra4RF_5pYZuyNJlIja16piesATnai66R4", sheet = "iit_summary")
    
    
# VIZ ============================================================================

  df_iit_extract %>% 
      filter(snu1 == "Copperbelt", trendscoarse == "<15") %>% 
      ggplot(aes(x = period, y = iit, color = already_running_on_sc)) +
      geom_point(position = position_jitter()) +
      facet_wrap(~facility) +
      si_style(facet_space = 0.25)

# SPINDOWN ============================================================================

