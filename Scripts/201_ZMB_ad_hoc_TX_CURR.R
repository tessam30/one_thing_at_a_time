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


df <- read_psd(msd_clean_path) %>% filter(funding_agency == "USAID")

source("Scripts/helper-call_all_helpers.R")


calc_tx <- function(.data){
.data %>% 
  filter(indicator %in% c("TX_CURR"), 
         standardizeddisaggregate %in% "Total Numerator") %>% 
  group_by(indicator, fiscal_year, mech_name, mech_code, prime_partner_name) %>% 
  summarise(across(.cols = c(cumulative), \(x) sum(x, na.rm = T), .names = "TX_CURR"), .groups = "drop")
}

df %>% swap_targets() %>% 
  filter(fiscal_year == 2023) %>% calc_tx() %>% View()

df %>% count(mech_name)
