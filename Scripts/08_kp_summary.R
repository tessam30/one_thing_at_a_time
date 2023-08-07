# PROJECT: FY23Q3 Review -- KP Summary
# PURPOSE: Munge and Analysis of KP data
# AUTHOR: Tim Essam | SI
# REF ID:   25005500
# LICENSE: MIT
# DATE: 2023-05-15
# NOTES: Tim Essam | SI

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
  library(selfdestructin5)

  source("Scripts/helper-call_all_helpers.R")


  # SI specific paths/functions  
  load_secrets()
  genie_path <- "Data/Genie-PSNUByIMs-Zambia-Daily-2023-07-26.zip"
  
  # Grab metadata
  get_metadata(genie_path)
  
  # REF ID for plots
  ref_id <- "0aea9a5a"
  
  # Functions  


# LOAD DATA ============================================================================  

  df_genie <- read_psd(genie_path)

# MUNGE ============================================================================

  df_kp <- df_genie %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW")) %>% 
    clean_indicator() %>% 
    filter(standardizeddisaggregate %in% c("KeyPop/Result", 
                                           "KeyPop/HIVStatus", 
                                           "KeyPop/Indication/HIVStatus"),
           fiscal_year == metadata$curr_fy) %>% 
    group_by(indicator, fiscal_year, otherdisaggregate) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = T), .groups = "drop") %>% 
    mutate(achv = cumulative / targets)
  
  
  df_kp_tx <- df_genie %>% 
    filter(indicator %in% c("TX_NEW", "TX_PVLS", "TX_CURR")) %>% 
    clean_indicator() %>% 
    filter(standardizeddisaggregate %in% c("KeyPop/HIVStatus", "KeyPop/Indication/HIVStatus"),
           fiscal_year == metadata$curr_fy, 
           otherdisaggregate != "MSM, Targeted") %>% 
    mutate(otherdisaggregate = str_replace_all(otherdisaggregate, ", Routine", "")) %>%
    group_by(indicator, fiscal_year, otherdisaggregate) %>% 
    summarise(across(c(targets, cumulative), \(x) sum(x, na.rm = T)), .groups = "drop") %>% 
    mutate(achv = cumulative / targets)
  
  df_kp_vl <- df_genie %>% 
    filter(indicator %in% c( "TX_CURR", "TX_PVLS")) %>% 
    clean_indicator() %>% 
    filter(standardizeddisaggregate %in% c("KeyPop/HIVStatus", "KeyPop/Indication/HIVStatus")) %>% 
    mutate(otherdisaggregate = str_replace_all(otherdisaggregate, ", Targeted|, Routine", "")) %>%
    group_by(indicator, fiscal_year, otherdisaggregate) %>% 
    summarise(across(matches("qtr"), \(x) sum(x, na.rm = T)), .groups = "drop") %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = indicator,
                values_from = "value") %>% 
    arrange(otherdisaggregate, period) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, n = 2), .by = otherdisaggregate,
           VLS = TX_PVLS / TX_PVLS_D,
           VLC = TX_PVLS_D / TX_CURR_LAG2)
  
# VIZ ============================================================================
  
  df_kp %>% 
    ggplot(aes(x = otherdisaggregate)) +
    geom_col(aes(y = targets), fill = grey20k, width = 0.6, position = position_nudge(x = -0.1)) +
    geom_col(aes(y = cumulative, fill = indicator), width = 0.6) +
    facet_wrap(~indicator, scales = "free_y") +
    geom_text(aes(y = cumulative, label = percent(achv, 1)),
              size = 12/.pt,
              family = "Source Sans Pro",
              color = grey90k,
              vjust = -0.25
    ) +
    scale_fill_si(palette = "siei", discrete = T) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    scale_y_continuous(labels = comma) +
    si_style_ygrid() +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL, 
         title = glue("KP Summary for {metadata$curr_pd}"),
         caption = glue("{metadata$caption}"))
  si_save(glue("Images/ZMB_{metadata$curr_pd}_kp_achv_by_disag.png"))
  
  
  num_pds <- df_kp_vl %>% 
    filter(period >= "FY22Q1") %>% 
    distinct(period) %>% 
    count() %>% 
    pull()
  
  df_kp_vl %>% 
    filter(period >= "FY22Q1") %>% 
    select(-c(TX_CURR, period_type, TX_PVLS, TX_PVLS_D, TX_CURR_LAG2)) %>% 
    rename(vls = VLS, vlc = VLC) %>% 
    # pivot_longer(cols = VLS:VLC,
    #              names_to = "VL",
    #              values_to = "values") %>% 
    # mutate(fill_color = ifelse(VL == "VLC", burnt_sienna, denim)) %>% 
    ggplot(aes(x = period, group = 1)) + 
    geom_line(aes(y = vls), color = burnt_sienna) +
    geom_point(aes(y = vls), shape = 21, fill = burnt_sienna, size = 3,
               color = "white") +
    geom_line(aes(y = vlc), color = denim) +
    geom_point(aes(y = vlc), shape = 21, fill = denim, size = 3,
               color = "white") +
    facet_wrap(~otherdisaggregate) +
    scale_color_identity() +
    geom_text(aes(y = vlc, label = percent(vlc, 1)), size = 9/.pt,
              family = "Source Sans Pro", color = denim, 
              vjust = -1) +
    geom_text(aes(y = vls, label = percent(vls, 1)), size = 9/.pt,
              family = "Source Sans Pro", color = burnt_sienna, 
              vjust = -1) +
    annotate("text", x = num_pds + .5, y = .97, label = "Viral Load\nSuppression",
             color = burnt_sienna, size = 10/.pt,
             hjust = 0.1, 
             family = "Source Sans Pro") +
    annotate("text", x = num_pds + .5, y = .69, label = "Viral Load\nCoverage",
             color = denim, size = 10/.pt,
             hjust = 0.1, 
             family = "Source Sans Pro") +
    si_style_ygrid() +
    expand_limits(x = c(1, num_pds+0.5), y = c(0.7,1.15)) +
    scale_y_continuous(labels = percent, breaks = seq(0.25, 1, 0.25)) +
    labs(x = NULL, y = NULL,
         title = glue("KP VIRAL LOAD COVERAGE & SUPPRESSION TRENDS"),
         caption = glue("{metadata$caption}"))
  # si_save(glue("Graphics/ZMB_{metadata$curr_pd}_kp_achv_by_disag_vl.svg"))

# Summary graph ============================================================================
  
  
  # Pull KP PREV first
  df_kp <- 
    df_genie %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG",
                            "KP_PREV", "PrEP_CT", "PrEP_CT", "PrEP_NEW"),
           standardizeddisaggregate %in% c("KeyPop", "KeyPop/HIVStatus", 
                                           "KeyPopAbr", "KeyPop/Result"),
           fiscal_year == metadata$curr_fy) %>% 
    clean_indicator() %>% 
    mutate(otherdisaggregate = case_when(
      str_detect(otherdisaggregate, "People in prisons") ~ "People in prisions",
      TRUE ~ otherdisaggregate
    )
    )
  
  df_kp_all <- df_kp %>% mutate(otherdisaggregate == "ALL KPs")  
  
  munge_kp <- function(df, ...){   
    df %>% 
      filter(fiscal_year == metadata$curr_fy) %>% 
      group_by(fiscal_year, indicator, otherdisaggregate, standardizeddisaggregate, ...) %>% 
      summarise(across(matches("targ|cumu"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(achv = cumulative / targets)
  }
  
  df_kp_viz  <- 
    df_kp %>% 
    bind_rows(., df_kp %>% mutate(otherdisaggregate = "ALL KPs")) %>% 
    filter(indicator != "PrEP_CT", otherdisaggregate != "PWID") %>% 
    munge_kp() %>% 
    mutate(indicator = fct_relevel(indicator, c("KP_PREV", "HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "PrEP_NEW")),
           indic_color = case_when(
             indicator == "KP_PREV" ~ "#88CCEE",
             indicator == "HTS_TST" ~ "#DDCC77",
             indicator == "HTS_TST_POS" ~ "#CC6677",
             indicator == "HTS_TST_NEG" ~ "#117733",
             indicator == "PrEP_NEW" ~ "#AA4499"
           ))
  
  df_kp_agency <- 
    df_kp %>% 
    #bind_rows(., df_kp %>% mutate(otherdisaggregate = "ALL KPs")) %>% 
    filter(indicator != "PrEP_CT", otherdisaggregate != "PWID") %>% 
    clean_agency() %>% 
    munge_kp(funding_agency) %>% 
    mutate(indicator = fct_relevel(indicator, c("KP_PREV", "HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "PrEP_NEW")),
           indic_color = case_when(
             indicator == "KP_PREV" ~ "#88CCEE",
             indicator == "HTS_TST" ~ "#DDCC77",
             indicator == "HTS_TST_POS" ~ "#CC6677",
             indicator == "HTS_TST_NEG" ~ "#117733",
             indicator == "PrEP_NEW" ~ "#AA4499"
           )) %>% 
    filter(funding_agency %in% c("CDC", "USAID"))

  df_kp_ip <- 
    df_kp %>% 
    filter(funding_agency == "USAID") %>% 
    munge_kp(mech_name, mech_code, snu1) %>%
    filter(otherdisaggregate == "People in prisions") %>% 
    fix_mech_names() %>% 
    mutate(indicator = fct_relevel(indicator, c("KP_PREV", "HTS_TST", "HTS_TST_POS", "HTS_TST_NEG", "PrEP_NEW")),
           indic_color = case_when(
             indicator == "KP_PREV" ~ "#88CCEE",
             indicator == "HTS_TST" ~ "#DDCC77",
             indicator == "HTS_TST_POS" ~ "#CC6677",
             indicator == "HTS_TST_NEG" ~ "#117733",
             indicator == "PrEP_NEW" ~ "#AA4499"
           ))
    
# VIZ ============================================================================
  
  top <- df_kp_viz %>% 
    filter(otherdisaggregate == "ALL KPs") %>%
    #filter(str_detect(mech_name, ("ACTION HIV|CHECKUPII"), negate = T)) %>% 
    mutate(cumulative = na_if(cumulative, NA_integer_)) %>% 
    ggplot(aes(y = indicator)) +
    geom_col(aes(x = targets), fill = grey20k, 
             position = position_nudge(y = -0.15), width = 0.5) +
    geom_col(aes(x = cumulative, fill = indic_color), width = 0.5) +
    geom_errorbar(aes(xmax=targets, xmin=targets), 
                  width=0.75, 
                  size = 0.75, 
                  color= "#ffffff", 
                  linetype = "dotted") +
    geom_text(aes(x = cumulative, label = percent(achv, 1)), 
              size = 10/.pt, 
              family = "Source Sans Pro", 
              hjust = 0) +
    # facet_wrap(mech_name~otherdisaggregate, ncol = 5) +
    facet_grid(. ~ otherdisaggregate, scales = "free_y", space = "free") +
    si_style_xgrid(facet_space = 1.1) +
    scale_fill_identity() +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = label_number_si(), position = "top", expand = c(0.15, 0.1)) +
    labs(x = NULL, y = NULL, title = glue("ZAMBIA KP ACHIEVEMENTS FOR {metadata$curr_pd}")) +
    #caption = metadata$caption) +
    theme(
      strip.placement = "outside",
      strip.text = element_text(face = "bold")
    ) +
    expand_limits(x = c(-0.1, 0))
  top
  
  bottom <- df_kp_agency %>% 
    #filter(str_detect(mech_name, ("DISCOVER|CHEKUP|Open|SAFE"))) %>% 
    #filter(otherdisaggregate != "ALL KPs") %>%
    ggplot(aes(y = indicator)) +
    geom_col(aes(x = targets), fill = grey20k, 
             position = position_nudge(y = -0.15), width = 0.5) +
    geom_col(aes(x = cumulative, fill = indic_color), width = 0.5) +
    geom_errorbar(aes(xmax=targets, xmin=targets), 
                  width=0.75, 
                  size = 0.75, 
                  color= "#ffffff", 
                  linetype = "dotted") +
    geom_text(aes(x = cumulative, label = percent(achv, 1)), 
              size = 10/.pt, 
              family = "Source Sans Pro", 
              hjust = 0) +
    #facet_grid(. ~ otherdisaggregate, scales = "free_y", space = "free") +
    facet_grid(funding_agency ~ otherdisaggregate, scales = "free_y", space = "free", switch = "y") +
    si_style_xgrid(facet_space = 1.1) +
    scale_fill_identity() +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = label_number_si(), position = "top", expand = c(0.15, 0.1)) +
    labs(x = NULL, y = NULL, title = str_to_upper("ACHIEVEMENT BY KEY POPULATION DISAGGREGATES ACROSS AGENCIES"),
         caption = metadata$caption) +
    theme(
      strip.placement = "outside",
      strip.text = element_text(face = "bold")
    )
  bottom
  
  si_save(glue("Images/{metadata$curr_fy}_KP_achv_agency.png"), scale = 1.25)
  
# SPINDOWN ============================================================================
  
  top / bottom 
  si_save(glue("Images/{metadata$curr_fy}KP_achv.png"), scale = 1.25)
  

# IP SUMMARY BY People in prisons -----------------------------------------
  
  df_kp_ip %>% 
    filter(str_detect(snu1, "NorthWestern", negate = T)) %>%
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    ggplot(aes(y = snu1)) +
    geom_col(aes(x = targets), fill = grey20k, 
             position = position_nudge(y = -0.15), width = 0.5) +
    geom_col(aes(x = cumulative, fill = indic_color), width = 0.5) +
    geom_errorbar(aes(xmax=targets, xmin=targets), 
                  width=0.75, 
                  size = 0.75, 
                  color= "#ffffff", 
                  linetype = "dotted") +
    geom_text(aes(x = cumulative, label = percent(achv, 1)), 
              size = 10/.pt, 
              family = "Source Sans Pro", 
              hjust = 0) +
    #facet_grid( ~ otherdisaggregate, scales = "free_y", space = "free") +
    facet_grid(indicator ~ mech_name_short, scales = "free_x", space = "free", switch = "y") +
    si_style_xgrid(facet_space = 1.1) +
    scale_fill_identity() +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = label_number_si(), position = "top", expand = c(0.15, 0.1)) +
    labs(x = NULL, y = NULL, title = str_to_upper("PEOPLE IN PRISONS ACHIEVEMENT ACROSS USAID PARTNERS AND INDICATORS"),
         caption = metadata$caption) +
    theme(
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 7, hjust = 0.25)
    )
  
  si_save(glue("Images/{metadata$curr_fy}_KP_achv_partner_prisoners.png"), scale = 1.25)

# Summary table of Agency goals -------------------------------------------

  make_gt_kp <- function(df, indic = "HTS_TST") {
    df %>% 
      filter(indicator == indic) %>% 
      select(-c(standardizeddisaggregate, fiscal_year, indic_color)) %>% 
      gt(rowname_col = "funding_agency", groupname_col = "otherdisaggregate") %>% 
      fmt_number(columns = c(targets, cumulative), 
                 decimals = 0) %>% 
      fmt_percent(columns = achv, 
                  decimals = 0) %>% 
      gt_theme_nytimes() %>% 
      tab_options(
        data_row.padding = px(0.5),
        source_notes.font.size = "small"
      ) %>% 
      tab_source_note(
        source_note = gt::md(glue::glue("{metadata$caption}"))
      ) %>% 
      tab_header(
        title = glue::glue("{indic} FY23 Q2 SUMMARY OF KP RESULTS BY AGENCY")
      ) %>% 
      tab_style(
        style = list(
          cell_text(weight = 700)
        ),
        locations = cells_body(
          rows = funding_agency == "USAID",
          columns = everything()
        )
      ) 
  }

  indic_list <- df_kp_agency %>% distinct(indicator) %>% pull()
  
  map(indic_list, ~make_gt_kp(df_kp_agency, indic = .x) %>% 
        gtsave(filename = glue("Images/{.x}_KP_agency_table.png")))
