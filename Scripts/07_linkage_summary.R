# PROJECT: one thing at a time
# PURPOSE: Munge and Analysis of Genie FY23Q3 data
# AUTHOR:  Tim Esssam | SI
# REF ID:  3b06bf60
# LICENSE: MIT
# DATE:   2023-07-27
# NOTES:  In the future, exclude community partners in graphic

# LOCALS & SETUP ===============================================================

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
    library(rcartocolor)
    
  # SI specific paths/functions  
    load_secrets()
    genie_path <- "Data/Genie-PSNUByIMs-Zambia-Daily-2023-07-26.zip"
    merdata <- file.path(glamr::si_path("path_msd"))
    msd_path_old <- return_latest(folderpath = file.path(merdata, "Archive"),
                                  pattern = "PSNU_IM_FY20-23.*Zambia.zip")
    
    
    site_path <- "Data/Genie-SiteByIMs-Zambia-Daily-2023-07-26.zip"
    
    
    # Grab metadata
    get_metadata(genie_path)

  # REF ID for plots
    ref_id <- "3b06bf60"
    
  # SNU list
    snu1_list <- c("Muchinga", "NorthWestern", "Lusaka", "Eastern")
    
  # Functions  
    # calculate shares
    add_shares <- function(.data, ...){
      .data %>% 
        select(-targets) %>% 
        spread(indicator, results) %>% 
        group_by(period, ...) %>% 
        mutate(share = HTS_TST / sum(HTS_TST, na.rm = T),
               share_check = sum(share),
               share_pos = HTS_TST_POS / sum(HTS_TST_POS, na.rm = T),
               share_pos_check = sum(share_pos)) %>% 
        ungroup() %>% 
        mutate(share_label = case_when(
          period %in% c(max(period), min(period))  ~ share, 
          TRUE ~ NA_real_
        ),
        share_label_pos = case_when(
          period %in% c(max(period), min(period))  ~ share_pos, 
          TRUE ~ NA_real_
        ), 
        snu1_order = fct_reorder2(snu1, HTS_TST, share_label, .desc = T)
        )
    }
    
    pull_bounds <- function(df, age = "<15", var, bound = ""){
      
      if(bound == "max"){
        df %>% 
          filter(trendscoarse == age) %>% 
          summarise(max = max({{var}})) %>% 
          pull()
      }
      
      else if(bound == "min"){
        df %>% 
          filter(trendscoarse == age) %>% 
          summarise(min = min({{var}})) %>% 
          pull()
      }
    }
    
    source("Scripts/helper-call_all_helpers.R")

# LOAD DATA ============================================================================  

    # What community partners should be excluded from the calculations?
    # Need to use the site level msd to exclude community sites from the calc
    # Purple bar, showing community partners and non-community partners
    
    # Exclude the community sites
    df_site <-  read_psd(site_path) %>% 
      filter(funding_agency == "USAID", 
             sitetype != "Community") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets()  
    
    
    
    
    
    df_genie %>% filter(indicator == "HTS_TST_POS", 
                        standardizeddisaggregate == "Total Numerator",
                        fiscal_year == 2023) %>% View()
    
    
    df_msd_old <- read_msd(msd_path_old) %>% 
      filter(fiscal_year %in% c(2020, 2021), funding_agency == "USAID")
    
    # bind these together b/c we need past TX_CURR to compute VLC
    df_genie <- read_psd(genie_path) %>% 
      filter(fiscal_year > 2021) %>% 
      bind_rows(df_msd_old) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets()   

# MUNGE ============================================================================

      df_hts_base <- 
        df_genie %>% 
        filter(indicator %in% c("HTS_TST_POS", "TX_NEW", "HTS_TST"), 
               standardizeddisaggregate == "Total Numerator",
               fiscal_year <= metadata$curr_fy, 
               funding_agency == "USAID") %>% 
        group_by(indicator, fiscal_year) %>% 
        summarise(across(targets:qtr4, sum, na.rm = T), .groups = "drop") %>% 
        reshape_msd(direction ="semi-wide") %>% 
        group_by(indicator) %>% 
        fill(targets, .direction = "down") %>% 
        filter(nchar(period) != 4) 
      
      df_hts_tgt <- df_hts_base %>% select(-results) %>% 
        pivot_wider(names_from = indicator, values_from = targets) %>% 
        mutate(linkage = TX_NEW / HTS_TST_POS)
      
      df_linkage <- 
        df_hts_base %>% 
        select(-targets) %>% 
        pivot_wider(names_from = indicator, values_from = results) %>% 
        mutate(linkage = TX_NEW / HTS_TST_POS)  
      
  
# VIZ ============================================================================

      bottom_hts <-  df_linkage %>% 
        ggplot(aes(x = period)) +
        geom_col(aes(y = HTS_TST_POS), fill = "#855C75",
                 position = position_nudge(x = 0.1), width = 0.5) +
        geom_col(aes(y = TX_NEW), fill = "#D9AF6B", 
                 position = position_nudge(x = -0.1), width = 0.5) +
        si_style_ygrid() +
        scale_y_continuous(labels = comma) +
        labs(x = NULL, y = NULL) +
        expand_limits(x = c(0, 9))  .
      
      # Linkage plot - #b08472
      top_hts <- df_linkage %>% 
        ggplot(aes(x = period, group = 1)) +
        geom_line(aes(y = linkage), color = grey50k, size = 0.5) +
        geom_point(aes(y = linkage), shape = 19, color = "#b08472",  size = 3) + 
        geom_point(aes(y = linkage), shape = 1, color = grey90k,  size = 3) + 
        geom_text(aes(y = linkage, label = percent(linkage, 1)), 
                  size = 9/.pt,
                  family = "Source Sans Pro",
                  fontface = "bold", 
                  color = "#b08472", 
                  vjust = -1.5) +
        si_style_nolines() +
        expand_limits(y = c(.85, 1), x = c(0, 9)) +
        theme(axis.text.y = element_blank(), 
              axis.text.x = element_blank()) +
        labs(x = NULL, y = NULL) +
        annotate("text", x = 15.5, y = 0.9, label = "Linkage", 
                 size = 11/.pt, color = "#b08472")
      
      top_hts / bottom_hts +
        plot_layout(heights = c(1, 4))
      si_save("Graphics/Linkage_summary.svg")

# OVERALL TESTING TRENDS ============================================================================

      df_hts_combo <- df_hts_base %>% 
        select(-targets) %>% 
        spread(indicator, results) %>% 
        mutate(positivity = HTS_TST_POS / HTS_TST)
      
      
      df_hts_combo %>% 
        ggplot(aes(x = period)) +
        geom_col(aes(y = HTS_TST), fill = "#e0d4db", width = 0.75,
                 position = position_nudge(x = 0.1)) +
        geom_col(aes(y = HTS_TST_POS), fill = "#855C75", width = 0.75) +
        geom_text(aes(y = HTS_TST_POS, label = percent(HTS_TST_POS/HTS_TST, 1)),
                  size = 11/.pt, 
                  family = "Source Sans Pro", 
                  color = grey90k,
                  vjust = -0.5) +
        #geom_col(aes(y = HTS_TST_cmltv), width = 0.5, fill = grey50k) +
        #geom_col(aes(y = HTS_TST_POS_cmltv), width = 0.5, fill = "#855C75") +
        si_style_ygrid() +
        scale_y_continuous(labels = label_number_si()) +
        labs(x = NULL, y = NULL, title = "TESTING REMAINS HIGH BUT POSITIVITY IS DOWN",
             caption = metadata$caption)+
        coord_cartesian(expand = F)
      si_save("Images/HTS_positivity_summary.png")
      
