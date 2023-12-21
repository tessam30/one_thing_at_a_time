# PROJECT: one thing at a time
# PURPOSE: Maps of Zambia Partner Coverage in FY23 & FY24
# AUTHOR:  Tim Esssam | SI
# REF ID:  6cd83849
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
  library(selfdestructin5)


  # SI specific paths/functions  
  load_secrets()
  si_path()
  msd_clean_path <- return_latest(folder = si_path(), pattern = "PSNU_IM.*Zambia")
  shpdata <- file.path(glamr::si_path("path_vector"))

  # Grab metadata
  get_metadata(msd_clean_path)
  
  # REF ID for plots
  ref_id <- "6cd83849"
  
  # Map infrastructure
  cntry <- "Zambia"
  spdf_pepfar <- gisr::get_vcpolygons(folderpath = shpdata, name = "VcPepfarPolygons.shp")
  zmb_org <- grabr::datim_orgunits(cntry = cntry) %>% filter(orgunit_level == 5)
  
  # May need to rework below depending on gisr updates (use zmb_org)
  zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                       level = .x))
  names(zmb_geo) <- list("adm0", "snu1", "psnu")
  
  zmb_adm1 <- st_read("../Zambezi/GIS/snu1_fy22.shp")
  
  # Functions  
  # Darken columns
  drkn_clmn_hdr <- function(.data){
    .data %>%  
      tab_style(
        style = list(
          cell_text(color = grey90k)
        ),
        locations = cells_column_labels(
        )
      )
  }
  
  # Collapse targets down to different disaggregates
  collapse_targets <- function(.data, indicator, ...) {
    df <-  .data %>% 
      filter(funding_agency == "USAID", indicator == {{indicator}},
             standardizeddisaggregate == "Total Numerator", 
             fiscal_year > 2022) %>% 
      #mech_code %in% c("17413", "82086", "17399", "86412")) %>% 
      group_by(fiscal_year, ...) %>% 
      summarise(targets = sum(targets, na.rm = T), .groups = "drop") 
    return(df)
  }  


# LOAD DATA ============================================================================  
  
  df_msd <- read_psd(msd_clean_path) %>% 
    apply_partner_type()
  
  # Load Helper functions
  
  source("Scripts/helper-call_all_helpers.R")
  
  
  # Check if ZIH really reported in Q3
  df_msd %>% 
    filter(mech_code == "86412", indicator %in% c("TX_CURR", "HTS_TST_POS", "TX_PVLS")) %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(cumulative = sum(cumulative, na.rm = T), 
              targets = sum(targets, na.rm = T))

# MUNGING -----------------------------------------------------------------

  # Extract TX_CURR Targets for past years by IPS & SNUs
  
  df_tgts_psnu <- collapse_targets(df_msd, "TX_CURR",  psnu, psnuuid, snu1) 
  
  
  # Show target differences across years
  df_tgts_snu <- df_tgts_psnu %>% 
    group_by(province = snu1, fiscal_year) %>% 
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = fiscal_year, 
                values_from = targets) %>% 
    mutate(delta = `2024` - `2023`) %>% 
    mutate(tx_curr_24_tot = sum(`2024`, na.rm = T), 
           snu1_share = `2024`/tx_curr_24_tot) %>% 
    arrange(desc(snu1_share)) %>% 
    select(-tx_curr_24_tot) 
  
df_tgts_hts <- collapse_targets(df_msd, "HTS_TST_POS", snu1)  %>% 
  pivot_wider(names_from = fiscal_year, 
              values_from = targets) %>% 
  mutate(delta = `2024` - `2023`) %>% 
  mutate(hts_24_tot = sum(`2024`, na.rm = T), 
         snu1_share = `2024`/hts_24_tot) %>% 
  arrange(desc(snu1_share)) %>% 
  select(-hts_24_tot) %>% 
  rename(province = snu1)

# GT TABLES of TARGETS ----------------------------------------------------

  df_tgts_snu %>% 
    mutate(province = str_remove_all(province, " Province")) %>% 
    gt() %>% 
    fmt_number(2:4, 
               decimals = 0) %>% 
    fmt_percent(5, 
                decimals = 0) %>% 
    sub_missing(missing_text = "-") %>% 
    tab_source_note(source_note = glue("{metadata$caption}")) %>% 
    gtExtras::gt_theme_nytimes() %>%
    drkn_clmn_hdr() %>% 
    cols_label(
      `2023` = "FY23",
      `2024` = "FY24",
      snu1_share = "FY24 Share"
    ) %>% 
   gt_hulk_col_numeric(columns = 4, trim = T) %>% 
    grand_summary_rows(columns = where(is.numeric),
      fns = list(
      overall = ~sum(., na.rm = T)),
      formatter = fmt_number,
      decimals = 0
    ) %>% 
    tab_header(title = str_to_upper("USAID Zambia TX_CURR Targets Increased by  nearly 60K in FY24"),
               subtitle = "Over 60% of USAID's TX_CURR targets are in Copperbelt and Central Province") %>% 
    gtsave("Images/ZMB_tx_curr_target_summary.png")
  
  
  # Testing Changes
df_tgts_hts %>% 
  mutate(province = str_remove_all(province, " Province")) %>% 
  filter(str_detect(province, "Western", negate = T)) %>% 
  gt() %>% 
  fmt_number(2:4, 
             decimals = 0) %>% 
  fmt_percent(5, 
              decimals = 0) %>% 
  sub_missing(missing_text = "-") %>% 
  tab_source_note(source_note = glue("{metadata$caption}")) %>% 
  gtExtras::gt_theme_nytimes() %>%
  drkn_clmn_hdr() %>% 
  cols_label(
    `2023` = "FY23",
    `2024` = "FY24",
    snu1_share = "FY24 Share"
  ) %>% 
  gt_hulk_col_numeric(columns = 4, trim = T) %>% 
  grand_summary_rows(columns = where(is.numeric),
                     fns = list(
                       overall = ~sum(., na.rm = T)),
                     formatter = fmt_number,
                     decimals = 0
  ) %>% 
  tab_header(title = str_to_upper("USAID Zambia HTS_TST_POS Targets Increased by 15K in FY24"),
             subtitle = "Over 60% of USAID's HTS_TST targets are in Copperbelt and Central Province")  %>% 
  gtsave("Images/ZMB_hts_tst_target_summary.png")  



# MAPPY MAP MAPs ----------------------------------------------------------

  df_tgts_snu_mech <- 
    df_msd %>% 
    fix_mech_names() %>% 
    filter(indicator %in% c("HTS_TST", "TX_CURR", "PrEP_NEW", "KP_PREV", "OVC_SERV"),
           standardizeddisaggregate == "Total Numerator", 
           fiscal_year > 2022) %>% 
    group_by(fiscal_year, funding_agency, mech_name_short, mech_name, mech_code, snu1, snu1uid, is_indigenous_prime_partner) %>% 
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>% 
    clean_agency() %>% 
    mutate(mech_code = ifelse(funding_agency == "CDC", "12345", mech_code),
           mech_type = case_when(
             mech_code %in% c("82075", "17399", "86412", "82075", "82086", "17413") ~ "Clinical", 
             mech_code %ni% c("82075", "17399", "86412", "82075", "82086", "17413") & funding_agency != "CDC" ~ "Community",
             TRUE ~ NA_character_
           )
    ) 


df_tgts_snu_mech %>% 
  left_join(zmb_adm1, by = c("snu1uid" = "snu1uid")) %>% 
  #mutate(mech_name = ifelse(funding_agency == "CDC", "CDC", mech_name)) %>% 
  filter(funding_agency == "USAID", mech_type == "Community") %>% 
  ggplot() +
  geom_sf(data = zmb_geo$adm0) + 
  geom_sf(aes(fill = mech_name_short, geometry = geometry), color = "white", stroke = 0.25) +
  geom_sf(data = zmb_adm1, aes(geometry = geometry), fill = NA, color = "white") + 
  geom_sf(data = zmb_geo$adm0, fill = NA, color = grey90k) + 
  facet_wrap(mech_name_short ~ fiscal_year,
             labeller = labeller(mech_name_short = label_wrap_gen(width = 25))) +
  si_style_map() +
  scale_fill_si(palette = "siei", discrete = T) +
  theme(legend.position = "none", 
        strip.text = element_text(size = 8)) +
  labs(title = "USAID Zambia Community Partner Coverage",
       caption = glue("{metadata$caption}"))
  si_save("Images/ZMB_Community_partner_coverage.png") 
  
  # NOTES: Toggle Local Partner Switch as needed
  df_tgts_snu_mech %>% 
    left_join(zmb_adm1, by = c("snu1uid" = "snu1uid")) %>% 
    #mutate(mech_name = ifelse(funding_agency == "CDC", "CDC", mech_name)) %>% 
    filter(funding_agency == "USAID", mech_type == "Clinical", is_indigenous_prime_partner == "Y") %>% 
    ggplot() +
    geom_sf(data = zmb_geo$adm0) + 
    geom_sf(aes(fill = mech_name_short, geometry = geometry), color = "white", stroke = 0.25) +
    geom_sf(data = zmb_adm1, aes(geometry = geometry), fill = NA, color = "white") + 
    geom_sf(data = zmb_geo$adm0, fill = NA, color = grey90k) + 
    facet_wrap(mech_name_short ~ fiscal_year, nrow = 1,
               labeller = labeller(mech_name_short = label_wrap_gen(width = 25))) +
    si_style_map() +
    scale_fill_si(palette = "scooter", discrete = T) +
    theme(legend.position = "none", 
          strip.text = element_text(size = 8)) +
    labs(title = "USAID Zambia Clinical Local Partner Coverage",
         caption = glue("{metadata$caption}"))
  si_save("Images/ZMB_Clinical_local_partner_coverage.png") 
    


df_msd %>% names()


  df_tgts_psnu_mech %>% 
    count(snu1, mech_name, fiscal_year, funding_agency) %>% 
    spread(fiscal_year, n) %>% prinf()
  
  
  df_tgt_geo <- 
    df_tgts_psnu %>% 
    left_join(zmb_geo$psnu, by = c("psnuuid" = "uid"))  

  df_tgt_geo %>% 
    filter(targets > 0) %>% 
    ggplot() + 
    geom_sf(data = zmb_geo$psnu, fill = grey20k, color = "white", stroke = 0.25) +
    geom_sf(aes(geometry = geometry, fill = targets)) +
    geom_sf(data = zmb_geo$adm0, fill = NA) +
    facet_wrap(~fiscal_year) +
    scale_fill_viridis_c(trans = "log", 
                         breaks = c(500, 1000, 5000, 10000, 25000, 50000),
                         labels = c(500, 1000, 5000, 10000, 25000, 50000)) +
    si_style_map() +
    si_legend_fill()
  
  
  df_tgts_psnu_mech_geo <-  
    df_tgts_psnu_mech %>% 
    left_join(zmb_geo$psnu, by = c("psnuuid" = "uid"))  
  
  df_tgts_psnu_mech_geo %>% distinct(mech_code)
  
  df_tgts_psnu_mech_geo %>% 
    filter(targets > 0) %>% 
    filter(mech_code == "86412") %>% 
    ggplot() + 
    geom_sf(data = zmb_geo$psnu, fill = grey20k, color = "white", stroke = 0.25) +
    geom_sf(aes(geometry = geometry, fill = targets), color = "white") +
    geom_sf(data = zmb_geo$adm0, fill = NA) +
    facet_wrap(mech_name~fiscal_year) +
    scale_fill_viridis_c() +
    si_style_map() +
    si_legend_fill()
  

# DUMBELL PLOTS TARGET CHANGES --------------------------------------------

  

  df_tgts_psnu %>% 
    filter(str_detect(snu1, "Southern", negate = T)) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province")) %>% 
    complete(psnu, fiscal_year) %>% 
    group_by(psnu) %>% 
    fill(psnuuid, snu1, .direction = "updown") %>% 
    ungroup() %>% 
    mutate(targets = ifelse(is.na(targets), 0, targets)) %>% 
    clean_psnu() %>% 
    mutate(psnu_order = tidytext::reorder_within(psnu, targets, snu1)) %>% 
    mutate(snu1_order = fct_reorder(snu1, targets, .desc = T)) %>% 
    ggplot(aes(y = psnu_order, color = factor(fiscal_year), shape = factor(fiscal_year))) +
    geom_line(aes(x = targets, group = psnu), color = grey50k) + 
    geom_point(aes(x = targets), size = 2.5) +
    facet_grid(snu1_order ~ ., space = "free", scales = "free_y", switch = "both") +
    scale_shape_manual(values = c(16, 18)) +
    scale_y_reordered() +
    si_style(facet_space = 0.25) +
    scale_color_manual(values = c('2023' = scooter_med, '2024' = old_rose)) +
    scale_x_continuous(labels = comma) +
    labs(x = NULL, y = NULL, title = "USAID ZAMBIA TX_CURR TARGETS BY PSNU") 
    
  
   

# EPI Control Investigation -----------------------------------------------

  # Checking Tableau Exec DB numbers
  data_source <- "../../../Downloads/PEPFAR Only - UNAIDS 2023 Clean Estimates - FINAL_2022 UNAIDS Estimates.csv"
  
  df_epi <- read_csv(data_source) %>% 
   filter(Country == "Zambia", Year == "2022")
  
  df_epi %>% names()
  
  
# FTF Flag  
  tmp <- c("Chibombo", "Kabwe", "Kapiri-Mposhi", "Luano", "Mkushi", "Nyimba", "Sinda", "Petauke", "Masaiti", "Mpongwe")
  paste(tmp, collapse ="|")
  
  zmb_org %>% 
    filter(str_detect(orgunit_name, "Chibombo|Kabwe|Kapiri-Mposhi|Luano|Mkushi|Nyimba|Sinda|Petauke|Masaiti|Mpongwe")) %>% 
    mutate(ftf = TRUE) %>% 
    write.csv("Data/tmp.csv")
                   