# PROJECT: one thing at a time
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  879dd403
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
    library(cascade)
    
    
  # SI specific paths/functions  
    load_secrets()
    genie_path <- "Data/Genie-PSNUByIMs-Zambia-Daily-2023-07-26.zip"

      
  # Grab metadata
   get_metadata(genie_path)
  
  # REF ID for plots
    ref_id <- "879dd403"
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_genie <- read_psd(genie_path) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets()
    
    # Load Helper functions
    
    source("Scripts/helper-call_all_helpers.R")

# MUNGE ============================================================================
  
    df_genie %>% filter(fiscal_year == metadata$curr_fy) %>% 
      distinct(mech_code, mech_name)
    
    # All of USAID Zambia cascade
    return_cascade(df_genie, 1) %>% prinf()
    
    batch_cascade_plot(df_genie %>% filter(funding_agency == "USAID"), 
                       imgpath = "Images/Cascade/USAID", imgtype = ".png")
    
    batch_cascade_plot(df_genie %>% filter(mech_name == "DISCOVER-H"),
                       imgpath = "Images/Cascade/DISCOVER", imgtype = ".png")
    
    batch_cascade_plot(df_genie %>% filter(mech_name == "SAFE"),
                       imgpath = "Images/Cascade/SAFE", imgtype = ".png")
    
    batch_cascade_plot(df_genie %>% filter(mech_name == "Action HIV"),
                       imgpath = "Images/Cascade/ACTION_HIV", imgtype = ".png")
    
    batch_cascade_plot(df_genie %>% filter(mech_name == "ZAM Health"),
                       imgpath = "Images/Cascade/ZAM Health", imgtype = ".png")
  
    # Copperbelt
    batch_cascade_plot(df_genie %>% filter(snu1 == "Copperbelt"), 
                       imgpath = "Images/Cascade/Copperbelt", imgtype = ".png")
