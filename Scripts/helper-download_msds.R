library(tidyverse)
library(glamr)
library(grabr)

# DIRS ----

dir_merdata <- si_path(type = "path_msd")

# PANO ACCESS ----

user <- pano_user()
pass <- pano_pwd()

sess <- grabr::pano_session(username = glamr::pano_user(), password = glamr::pano_pwd())

# OU
cntry = "Zambia"

# Extract all global & Country Specific MSD
pano_extract_msds(operatingunit = cntry,
                  archive = TRUE,
                  dest_path = dir_merdata,
                  username = user,
                  password = pass)
