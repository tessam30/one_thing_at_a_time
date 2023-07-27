# PROJECT: one thing at a time 
# PURPOSE: Munge and Analysis of genie FY23Q3 
# AUTHOR:  Tim Esssam | SI
# REF ID:  9c84ad8e
# LICENSE: MIT
# DATE:   2023-07-27
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
    library(rcartocolor)
    
    
  # SI specific paths/functions  
    load_secrets()

    genie_path <- "Data/Genie-PSNUByIMs-Zambia-Daily-2023-07-26.zip"
    merdata <- file.path(glamr::si_path("path_msd"))
    msd_path_old <- return_latest(folderpath = file.path(merdata, "Archive"),
                                  pattern = "PSNU_IM_FY20-23.*Zambia.zip")
    
    # Grab metadata
    get_metadata(genie_path)
  
  
  # REF ID for plots
    ref_id <- "9c84ad8e"
    
  # Functions  
    # Functions  
    munge_modality <- function(df, ...){   
      df_hts_full <- df %>% 
        filter(indicator == "HTS_TST_POS",
               standardizeddisaggregate == "Modality/Age/Sex/Result",
               fiscal_year <= metadata$curr_fy, 
               funding_agency == "USAID", ...) %>% 
        mutate(mod_type = case_when(
          str_detect(modality, "Index") ~ "Index",
          str_detect(modality, "OtherPITC") ~ "Other PITC",
          str_detect(modality, "PMTCT") ~ "PMTCT",
          modality == "VCT" ~ "VCT",
          str_detect(modality, "SNSMod") ~ "Community SNS",
          TRUE ~ "Other")
        ) %>%
        group_by(fiscal_year, mod_type, mech_name) %>%
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
        ungroup() %>%
        reshape_msd() %>%
        select(-period_type) %>%
        group_by(period) %>%
        mutate(contribution = value/sum(value)) %>%
        ungroup() %>%
        mutate(start = case_when(period == min(period) ~ contribution),
               end = case_when(period == max(period) ~ contribution)) %>%
        mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
        complete(mod_type, period, mech_name) %>% 
        group_by(mod_type, mech_name) %>% 
        fill(mod_order, .direction = "up") %>% 
        group_by(period, mech_name) %>% 
        mutate(pd_tot = sum(value, na.rm = T), 
               pd_25 = pd_tot * 0.25, 
               pd_50 = pd_tot * 0.5,
               pd_75 = pd_tot * 0.75) %>% 
        ungroup() %>% 
        mutate(mod_color = case_when(
          mod_type == "Index" ~ "#855C75", 
          mod_type == "VCT" ~ "#D9AF6B",
          mod_type == "Other PITC" ~ "#AF6458",
          mod_type == "PMTCT"~ "#736F4C",
          mod_type == "Community SNS" ~ "#526A83",
          TRUE ~ "#7C7C7C"
        ),
        note = case_when(
          mod_type == "Index" & period == "FY21Q1" ~ "HTS_TST_POS",
          TRUE ~ NA_character_
        )) %>% 
        filter(!is.na(mod_order))
      return(df_hts_full)
    }
    
    plot_modality <- function(df){
      
      mech_name <- df %>% distinct(mech_name) %>% pull()
      
      df %>% 
        ggplot(aes(x = period)) +
        geom_col(aes(y = pd_tot), fill = grey20k) +
        geom_col(aes(y = value, fill = mod_color)) +
        geom_errorbar(aes(ymin = pd_25, ymax = pd_25), 
                      size = 0.25, color = "white", 
                      linetype = "dotted") +
        geom_errorbar(aes(ymin = pd_50, ymax = pd_50), 
                      size = 0.25, color = "white", 
                      linetype = "dotted") +
        geom_errorbar(aes(ymin = pd_75, ymax = pd_75), 
                      size = 0.25, color = "white", 
                      linetype = "dotted") +
        scale_fill_identity() +
        facet_wrap(~mod_order) +
        geom_text(aes(y = value, label = percent(start, 1)), size = 7/.pt, vjust = -0.5) +
        geom_text(aes(y = value, label = percent(end, 1)), size = 7/.pt,  vjust = -0.5) +
        geom_text(aes(y = pd_tot, label = note), size = 8/.pt, color = "#7C7C7C",
                  hjust = 0.2, vjust = -0.25) +
        labs(x = NULL, y = NULL,
             title = glue("HTS MODALITY BY {mech_name} "),
             caption = glue("Source: {metadata$caption}")) +
        theme(legend.position = "none") +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                    "FY22Q1", "", "", "", 
                                    "FY22Q3", "", "")) +
        si_style_ygrid(facet_space = 0.5)  
    }
    
    # Now can crank out partner plots
    batch_modality_plot <- function(df, ip_code, export = TRUE){
      
      mech_name <- df %>% 
        filter(mech_code == ip_code) %>% 
        distinct(mech_name) %>% 
        pull()
      
      print(mech_name)    
      munge_modality(df, mech_code == ip_code) %>% 
        plot_modality(.)
      
      if(export == TRUE)
        si_save(glue("Graphics/HTS_modality_{mech_name}.svg"))
    }
    
    source("Scripts/helper-call_all_helpers.R")

# LOAD DATA ============================================================================  
   
     df_genie_in <- read_psd(genie_path)  %>% 
      filter(funding_agency == "USAID", fiscal_year >2021)
    
    df_msd <- read_psd(msd_path_old) %>% 
      filter(fiscal_year %in% c(2021), funding_agency == "USAID")
    
    # bind these together b/c we need past TX_CURR to compute VLC
    df_genie <- df_genie_in %>% 
      bind_rows(df_msd) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets() 
    

# MUNGE ============================================================================
  
    munge_modality(df_genie %>% mutate(mech_code = "123456", mech_name = "USAID"), 
                   mech_code == "123456") %>% 
      filter(str_detect(period, "FY20", negate = T)) %>% 
      plot_modality(.)
    
    si_save("Graphics/Index_testing_summary_by_modality.png")
  
# PARTNERS ============================================================================

    partner_list <- df_genie %>% 
      filter(str_detect(mech_name, "Action|DISCOVER|SAFE|ZAM")) %>% 
      distinct(mech_code) %>% pull()
    
    map(partner_list, ~batch_modality_plot(df_genie, .x))

# SPINDOWN ============================================================================

