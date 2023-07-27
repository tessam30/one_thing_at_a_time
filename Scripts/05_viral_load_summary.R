# PROJECT: one thing at a time
# PURPOSE: Munge and Analysis of Genie FY23Q3
# AUTHOR:  Tim Esssam | SI
# REF ID:  fb194c73
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
    
    
  # SI specific paths/functions  
    load_secrets()
    genie_path <- "Data/Genie-PSNUByIMs-Zambia-Daily-2023-07-26.zip"
    merdata <- file.path(glamr::si_path("path_msd"))
    msd_path_old <- return_latest(folderpath = file.path(merdata, "Archive"),
                                  pattern = "PSNU_IM_FY20-23.*Zambia.zip")
      
  # Grab metadata
   get_metadata(genie_path)

   
  # REF ID for plots
    ref_id <- "fb194c73"
  
  # Functions  
    pull_last_val <- function(df, var){
      df %>% 
        filter(period == max(period)) %>% 
        select({{var}}) %>% 
        pull()
    }
    
    # Grab gap between TX_CURR_LAG2 & TX_PVLS_D
    vl_gap <-  function(df){
      df <- df %>% 
        filter(period == max(period)) %>% 
        transmute(gap = tx_curr_lag2 - tx_pvls_d) %>% 
        pull(gap)
    }
    
    # Mech names order
    mech_names_order <- c("SAFE", "Action HIV", "DISCOVER-H", "ZAM Health")
    
    source("Scripts/helper-call_all_helpers.R")
    
  

# LOAD DATA ============================================================================  

    df_msd <- read_psd(msd_path_old) %>% 
      filter(fiscal_year %in% c(2020, 2021), funding_agency == "USAID")
    
    # bind these together b/c we need past TX_CURR to compute VLC
    df_genie <- read_psd(genie_path) %>% 
      filter(fiscal_year >= 2022) %>% 
      bind_rows(df_msd) %>% 
      filter(funding_agency == "USAID") %>% 
      fix_mech_names() %>% 
      mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
      clean_agency() %>% 
      swap_targets() 
    
    df_genie %>% count(funding_agency)
    df_genie %>% count(mech_name, mech_code)


# MUNGE ============================================================================
  
    df_vl <- df_genie %>% 
      filter(funding_agency == "USAID") %>% 
      create_vl_df() %>% 
      filter(str_detect(period, "20", negate = T))
    
    df_vl_peds <- df_genie %>% 
      filter(funding_agency == "USAID", trendscoarse == "<15") %>% 
      create_vl_df() %>% 
      filter(str_detect(period, "20", negate = T))
    
    df_vl_ip <- df_genie %>% 
      filter(funding_agency == "USAID",
             mech_name %ni% c("ZIHA")) %>%
      create_vl_df(mech_name) %>% 
      ungroup() %>% 
      filter(str_detect(period, "20", negate = T)) %>% 
      mutate(mech_name = fct_relevel(mech_name, mech_names_order))
    
  
# VIZ ============================================================================

    # Modify below depending on whether it is peds or all
    
    num_pds <- length(unique(df_vl$period))
    end_vls <- pull_last_val(df_vl, vls)
    end_vlc <- pull_last_val(df_vl, vlc)
    gap <- vl_gap(df_vl)
    end_tx_pvls <- pull_last_val(df_vl, tx_pvls_d)
    end_tx_lag <- pull_last_val(df_vl, tx_curr_lag2)


# PEDS --------------------------------------------------------------------

    top <- df_vl %>% 
      ggplot(aes(x = period, group = 1)) +
      geom_line(aes(y = vls), color = burnt_sienna) +
      geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
                 color = "white") +
      geom_line(aes(y = vlc), color = denim) +
      geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                 color = "white") +
      geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = denim, 
                vjust = -1) +
      geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = burnt_sienna, 
                vjust = -1) +
      annotate("text", x = num_pds + .5, y = end_vls, label = "Viral Load\nSuppression",
               color = burnt_sienna, size = 12/.pt,
               hjust = 0.1, 
               family = "Source Sans Pro Bold") +
      annotate("text", x = num_pds + .5, y = end_vlc, label = "Viral Load\nCoverage",
               color = denim, size = 12/.pt,
               hjust = 0.1, 
               family = "Source Sans Pro Bold") +
      si_style_nolines() +
      expand_limits(x = c(1, num_pds + 2), y = c(0.7, 1.05)) +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL)
    
    
    
    # Need TX_CURR_lag2 - TX_PVLS_D
    
    bottom <- df_vl %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = tx_curr_lag2), fill = grey10k, width = 0.75) +
      geom_col(aes(y = tx_pvls_d), fill = denim, width = 0.75) +
      si_style_ygrid() +
      scale_y_continuous(labels = comma, expand = c(0, 0)) +
      coord_cartesian(clip = "off") +
      expand_limits(x = c(1, num_pds + 2)) +
      labs(x = NULL, y = NULL) +
      annotate("segment", x = num_pds + .45, xend = num_pds + .45, y = end_tx_pvls, yend = end_tx_pvls + gap, 
               color = grey50k, size = 1.2) +
      annotate("text", x = num_pds + .65, y = (gap / 2) + end_tx_pvls, label = "Coverage gap", 
               hjust = 0, size = 12/.pt, family = "Source Sans Pro Black", 
               color = grey70k) +
      annotate("text", x = num_pds + .55, y = end_tx_lag, label = "TX_CURR_LAG2", 
               size = 10/.pt, family = "Source Sans Pro Bold", color = grey70k,
               hjust = 0) +
      annotate("text", x = num_pds + .55, y = end_tx_pvls, label = "TX_PVLS_D", 
               size = 10/.pt, family = "Source Sans Pro Bold", color = denim, 
               hjust = 0) +
      labs(caption = metadata$caption)
    
    top / bottom + plot_layout(heights = c(1, 3)) +
      plot_annotation(title = glue("USAID VIRAL LOAD SUMMARY THROUGH {metadata$curr_pd}")) &
      theme(plot.tag = element_text(family = "Source Sans Pro"))    
    
    si_save("Images/VL_summary_2023.png", scale = 1.25)
    

# BY IP -------------------------------------------------------------------

    num_pds <- length(unique(df_vl$period))
    end_vls <- pull_last_val(df_vl_ip, vls)
    end_vlc <- pull_last_val(df_vl_ip, vlc)
    gap <- vl_gap(df_vl_ip)
    end_tx_pvls <- pull_last_val(df_vl_ip, tx_pvls_d)
    end_tx_lag <- pull_last_val(df_vl_ip, tx_curr_lag2)
    
    top_ip <- 
      df_vl_ip %>% 
      complete(mech_name, period) %>% 
      filter(mech_name %ni% c("ZIHA", "CHEKUP II")) %>% 
      ggplot(aes(x = period, group = 1)) +
      geom_line(aes(y = vls), color = burnt_sienna) +
      geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
                 color = "white") +
      geom_line(aes(y = vlc), color = denim) +
      geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                 color = "white") +
      geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = denim, 
                vjust = -1) +
      geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = burnt_sienna, 
                vjust = -1) +
      si_style_nolines(facet_space = 0.5) +
      facet_wrap(~mech_name, nrow = 1) +
      theme(axis.text.y = element_blank(), 
            axis.text.x = element_blank()) +
      labs(x = NULL, y = NULL) +
      expand_limits(y = c(0.7, 1.05)) 
    
    bottom_ip <- 
      df_vl_ip %>% 
      complete(mech_name, period) %>% 
      filter(mech_name %ni% c("ZIHA", "CHEKUP II")) %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = tx_curr_lag2), fill = grey10k) +
      geom_col(aes(y = tx_pvls_d), fill = denim) +
      si_style_ygrid(facet_space = 0.5) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL) +
      facet_wrap(~mech_name, nrow = 1) +
      scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                  "FY22Q1", "", "", "", 
                                  "FY23Q1", "", "")) +
      coord_cartesian(expand = F) +
      #get rid of facet labels
      theme(strip.text.x = element_blank()) +
      labs(caption = metadata$caption)
    
    top_ip / bottom_ip + plot_layout(heights = c(1, 3)) +
      plot_annotation(title = glue("VIRAL LOAD SUMMARY FOR {metadata$curr_fy} BY PARTNER")) &
      theme(plot.tag = element_text(family = "Source Sans Pro"))
    
    si_save("Images/VL_summary_partners.png", scale = 1.25)    
    
# BY PROVINCE ============================================================================
    
    df_vl_snu1 <- df_genie %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")) %>% 
      clean_indicator() %>% 
      group_by(snu1, indicator, fiscal_year) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = indicator,
                  names_glue = "{tolower(indicator)}") %>% 
      arrange(snu1, period) %>% 
      group_by(snu1) %>% 
      mutate(tx_curr_lag2 = lag(tx_curr, n = 2),
             vlc = tx_pvls_d / tx_curr_lag2,
             vls = tx_pvls / tx_pvls_d,
             vls_adj = tx_pvls / tx_curr_lag2) %>% 
      ungroup()
    
    
    df_vl_snu1 %>% 
      filter(snu1 %ni% c("Lusaka", "Southern", "Eastern"),
             str_detect(period, "20", negate = T)) %>% 
      complete(snu1, period) %>% 
      ggplot(aes(x = period, group = 1)) +
      geom_line(aes(y = vls), color = burnt_sienna) +
      geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
                 color = "white") +
      geom_line(aes(y = vlc), color = denim) +
      geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
                 color = "white") +
      geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = denim, 
                vjust = -1) +
      geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
                family = "Source Sans Pro", color = burnt_sienna, 
                vjust = -1) +
      scale_x_discrete(labels = c("FY21Q1", "", "", "",
                                  "FY22Q1", "", "", "", 
                                  "FY23Q1", "", "")) +
      facet_wrap(~snu1) +
      #scale_x_discrete(breaks = every_nth(n = 4)) +
      si_style_nolines(facet_space = 0.5) +
      expand_limits(x = c(1, 10), y = c(0.7,1.05)) +
      theme(axis.text.y = element_blank()) +
      labs(x = NULL, y = NULL) +
      labs(title = "USAID VIRAL LOAD COVERAGE AND VIRAL LOAD SUPPRESSION TRENDS", 
           caption = glue("{metadata$caption}"))
    si_save("Images/ZMB_vls_vlc_by_snu1.png")        
    
    
    
    # BY AGE AND SEX

# AGE AND SEX -------------------------------------------------------------

    
    df_vl <- df_genie %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_CURR","TX_CURR_Lag2", "TX_PVLS"),
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
             ageasentered != "Unknown Age") %>% 
      filter(snu1 %ni% c("Lusaka", "Southern", "Eastern")) %>% 
      mutate(ageasentered = case_when(trendscoarse == "<15" ~ trendscoarse,
                                      ageasentered %in% c("50-54","55-59", "60-64", "65+") ~ "50+",
                                      TRUE ~ ageasentered)) %>% 
      clean_indicator() %>% 
      group_by(snu1, indicator, ageasentered, fiscal_year) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = indicator,
                  names_glue = "{tolower(indicator)}")
    
    df_vl <- df_vl %>% 
      arrange(snu1, ageasentered, period) %>% 
      group_by(snu1, ageasentered) %>% 
      mutate(tx_curr_lag2 = lag(tx_curr, n = 2),
             vlc = tx_pvls_d/tx_curr_lag2,
             vls = tx_pvls / tx_pvls_d,
             vls_adj = tx_pvls / tx_curr_lag2) %>% 
      ungroup() %>% 
      filter(period == max(period))
    
    df_avg <- df_vl %>% 
      group_by(snu1) %>% 
      summarise(avg_vlc = mean(vlc),
                avg_vls = mean(vls),
                avg_vls_adj = mean(vls_adj)) %>% 
      ungroup()
    
    
    df_usaid_vl <- df_vl %>% 
      summarise(vlc = sum(tx_pvls_d, na.rm = TRUE)/sum(tx_curr_lag2, na.rm = TRUE),
                vls = sum(tx_pvls, na.rm = TRUE) / sum(tx_pvls_d, na.rm = TRUE),
                vls_adj = sum(tx_pvls, na.rm = TRUE) / sum(tx_curr_lag2, na.rm = TRUE))
    
    
    df_vl %>% 
      mutate(tx_curr_pct = 1,
             fill_color = case_when(
               vlc < 0.75 ~ .95,
               TRUE ~ 0.75
             )) %>% 
      ggplot(aes(tx_curr_pct, ageasentered)) +
      geom_vline(data = df_avg, aes(xintercept = avg_vlc, color = grey80k)) +
      geom_col(fill = grey10k, alpha = 0.6) +
      geom_col(aes(vls), fill = denim, alpha = 0.65, width = 0.75) +
      geom_col(aes(vlc, alpha = fill_color), fill = burnt_sienna, width = 0.75) +
      geom_vline(xintercept = 1, linewidth = 0.5, color = grey30k) +
      geom_vline(xintercept = 0, linewidth = 0.5, color = grey30k) +
      geom_richtext(aes(vls, label = glue("<span style='color:white'>{percent(vls, 1)}</span>")),
                    label.color = NA, fill = NA,
                    nudge_x = -0.04, size = 3,
                    family = "Source Sans Pro") +
      geom_richtext(aes(vlc, label = glue("<span style='color:white'>{percent(vlc, 1)}</span>")),
                    label.color = NA, fill = NA,
                    nudge_x = -0.04, size = 3,
                    family = "Source Sans Pro") +
      geom_text(data = df_avg, aes(x = avg_vlc, y = avg_vlc, label = percent(avg_vlc, 1)),
                hjust = .3, vjust = 1.6,
                family = "Source Sans Pro Bold",
                color = grey90k, size = 10/.pt) +
      facet_wrap(~snu1) +
      coord_cartesian(expand = T, clip = "off") +
      scale_color_identity() +
      scale_alpha_identity() +
      scale_x_continuous(label = percent) +
      labs(x = NULL, y = NULL, 
           title = glue("In {metadata$curr_pd}, USAID VLC and VLS are at {percent(df_usaid_vl$vlc, 1)} and {percent(df_usaid_vl$vls, 1)} respectively") %>% toupper,
           subtitle = glue("<span style='color:{burnt_sienna}'>**VLC**</span> and <span style='color:{denim}'>**VLS**</span> rates"),
           caption = glue("Source: {metadata$source}")) +
      si_style_nolines() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown(),
            axis.text.x = element_blank()) 
    
    si_save("Images/ZMB_vls_vlc_by_snu_age.png")   
    
    

