# PROJECT: Showing historical coverage of USAID/CDC regions across time
# PURPOSE: Munge and Analysis of NAT_SUBNAT dtaa
# AUTHOR:  Tim Esssam | SI
# REF ID:  16ef30a9
# LICENSE: MIT
# DATE:   2023-12-08
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(sf)
    library(tameDP)

# Data Notes
# DATIM data as of: 11/15/2023, 21:40:37 UTC
# Genie report updated: 11/17/2023, 00:09:07 UTC
# Current period(s): 2021 Q1, 2021 Q2, 2021 Q3, 2021 Q4, 2021 Target, 2022 Q1, 2022 Q2, 2022 Q3, 2022 Q4, 2022 Target, 2023 Q1, 2023 Q2, 2023 Q3, 2023 Q4, 2023 Target, 2024 Target

    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    subnat_1 <- return_latest(folderpath = merdata,
      pattern = "NAT_SUBNAT.*FY15-20")
    
    subnat_2 <- return_latest(folderpath = merdata,
                              pattern = "NAT_SUBNAT.*FY21-24")
    
    ou_im <- return_latest(folderpath = merdata,
                           "PSNU.*Zambia-Frozen")  
    
    data_pack <- list.files(path = "../../../Downloads/", pattern = "Target Setting Tool_1150pm.xlsx", full.names = T)
    
  # Grab metadata
   get_metadata(subnat_2)
  
  # REF ID for plots
    ref_id <- "16ef30a9"
    
  # Functions  
  collapse_plhiv <- function(.data, indic = "PLHIV"){
    .data %>% 
      filter(indicator %in% indic, 
             standardizeddisaggregate %in% "Total Numerator") %>% 
      group_by(indicator, snu1, fiscal_year) %>% 
      summarise(across(.cols = c(targets), \(x) sum(x, na.rm = T), .names = "PLHIV"), .groups = "drop")
  }
  
  collapse_tx <- function(.data){
    .data %>% 
    filter(indicator %in% c("TX_CURR_SUBNAT"), 
         standardizeddisaggregate %in% "Total Numerator") %>% 
    group_by(indicator, snu1, fiscal_year) %>% 
    summarise(across(.cols = c(qtr4), \(x) sum(x, na.rm = T), .names = "TX_CURR_SUBNAT"), .groups = "drop")
  }
  

# LOAD DATA ============================================================================  

  subnat_fy20 <- read_psd(subnat_1) %>% filter(operatingunit == "Zambia")
  subnat_fy20 %>% count(fiscal_year)
  
  subnat_fy24 <- read_psd(subnat_2) %>% filter(operatingunit == "Zambia")
  
  psnu_snu_cw <- read_psd(ou_im) %>% distinct(psnu, psnuuid, snu1, snu1uid)
  
  ou_im_genie <- read_psd(ou_im) %>% 
    summarize(TX_CURR = sum(cumulative, na.rm = T), .by = c(snu1, fiscal_year)) %>% 
    arrange(snu1, fiscal_year)
  
  dp <- tameDP::tame_dp(data_pack, type = "SUBNAT") 
  
  
  %>% 
    filter(indicator == "PLHIV") %>% 
    left_join(psnu_snu_cw) %>% 
    summarise(PLHIV = sum(targets, na.rm = T), .by = snu1)
  
  
  
# MUNGE ============================================================================
  
  fy20 <- subnat_fy20 %>% 
    collapse_plhiv()
  
  # Data appear to be jacked up in 2020
  fy20_tx <- subnat_fy20 %>% 
    collapse_tx()
  
  fy24 <- subnat_fy24 %>% 
    collapse_plhiv()
  
  fy24_tx <- subnat_fy24 %>% 
    collapse_tx()

  
  plhiv_subnat <- fy24 %>% select(-indicator) %>% 
    mutate(fiscal_year = fiscal_year - 1) %>% 
    left_join(fy24_tx) %>% 
    mutate(gap = PLHIV - TX_CURR_SUBNAT, 
           coverage = TX_CURR_SUBNAT/PLHIV)
  
    
  
  plhiv <- bind_rows(fy20, fy24) %>% 
    mutate(fiscal_year = fiscal_year - 1) %>% 
    full_join(., ou_im_genie) %>% 
    mutate(gap = PLHIV - TX_CURR, 
           coverage = TX_CURR/PLHIV)
  
  
# VIZ ============================================================================

  min(plhiv$gap)
  max(plhiv$gap, na.rm = T)
  
  plhiv_subnat %>% 
    mutate(snu1 = fct_reorder(snu1, TX_CURR_SUBNAT, .desc = T)) %>% 
    filter(str_detect(snu1, "Milit", negate = T), 
           fiscal_year > 2020) %>% 
    ggplot(aes(x = factor(fiscal_year))) +
    geom_col(aes(y = PLHIV), fill = grey30k, width = 0.5, position = position_nudge(x = -0.1)) +
    geom_col(aes(y = TX_CURR_SUBNAT), fill = "#35978F", width = 0.5, position = position_nudge(x = 0.1)) +
    geom_label(aes(y = -10000, label = comma(gap), fill = gap), size = 10/.pt, color = grey90k) +
    geom_label(aes(y = TX_CURR_SUBNAT, label = percent(coverage, 1)), size = 8/.pt, color = grey90k,
              position = position_nudge(x = 0.1), family = "Source Sans Pro", 
              vjust = 1.25) +
    geom_text(aes(y = TX_CURR_SUBNAT, label = comma(TX_CURR_SUBNAT)), size = 10/.pt, color = grey90k,
              position = position_nudge(x = 0.1), family = "Source Sans Pro", 
              vjust = -0.5) +
    facet_wrap(~snu1, nrow = 2) +
    si_style_ygrid(facet_space = 0.75) +
    scale_y_continuous(labels = comma, breaks = seq(5e4, 4e5, 5e4)) +
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "BrBG")), limits = c(-86000, 86000), oob = scales::squish) +
    labs(x = NULL, y = "TX_CURR", title = "DATIM HISTORICAL PLHIV AND TX_CURR_SUBNAT SUMMARIES BY SNU1",
         subtitle = "Grey bar represented PLHIV\nGreen bar TX_CURR_SUBNAT as reported in DATIM\nPLHIV gap below bars in box",
         caption = glue("{metadata$caption}")) +
    theme(legend.position = "none")
  si_save("Graphics/ZMB_TX_CURR_SUBNAT_GAP.svg", scale = 1.25)
  
  
  plhiv %>% 
    mutate(snu1 = fct_reorder(snu1, TX_CURR, .desc = T),
           gap_color = ifelse(gap > 50000, "white", grey90k)) %>% 
    filter(str_detect(snu1, "Milit", negate = T), 
           fiscal_year > 2018) %>% 
    ggplot(aes(x = factor(fiscal_year))) +
    geom_col(aes(y = PLHIV), fill = grey30k, width = 0.5, position = position_nudge(x = -0.1)) +
    geom_col(aes(y = TX_CURR), fill = "#35978F", width = 0.5, position = position_nudge(x = 0.1)) +
    geom_label(aes(y = -10000, label = comma(gap), fill = gap, color = gap_color), size = 6/.pt) +
    geom_label(aes(y = TX_CURR, label = percent(coverage, 1)), size = 5/.pt, color = grey90k,
               position = position_nudge(x = 0.1), family = "Source Sans Pro", 
               vjust = 1) +
    geom_text(aes(y = TX_CURR, label = comma(TX_CURR)), size = 7/.pt, color = grey90k,
              position = position_nudge(x = 0.1), family = "Source Sans Pro", 
              vjust = -0.5) +
    facet_wrap(~snu1, nrow = 2) +
    si_style_ygrid(facet_space = 0.75) +
    scale_color_identity() +
    scale_y_continuous(labels = comma, breaks = seq(5e4, 4e5, 5e4)) +
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "BrBG")), limits = c(-86000, 86000), oob = scales::squish) +
    labs(x = NULL, y = "TX_CURR", title = "DATIM HISTORICAL PLHIV AND TX_CURR SUMMARIES BY SNU1",
         subtitle = "Grey bar represented PLHIV\nGreen bar TX_CURR as reported in DATIM\nPLHIV gap below bars in box",
         caption = glue("Source FY23Q4i MSD & FY23Q4i NAT_SUBNAT MSD")) +
    theme(legend.position = "none")
  si_save("Graphics/ZMB_TX_CURR_GAP.png", scale = 1.25)

# SPINDOWN ============================================================================

