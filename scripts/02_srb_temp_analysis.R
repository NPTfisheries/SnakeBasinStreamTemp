# -----------------------
# Author: Mike Ackerman
# Purpose:
#
#
# Created: December 19, 2024
#   Last Modified:
#
# Notes:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(sf)
library(here)

# set default crs for project
default_crs = st_crs(32611) # WGS 84, UTM zone 11N

#--------------------
# load and prep data

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  st_transform(default_crs) ; rm(spsm_pop)

# snake river basin 200m reach stream network w/ habitat and norwest attributes
load(here("data/spatial/srb_200m_rch_w_norwest.rda"))

# trim down but include norwest, data
srb_norwest_sf = srb_rch_sf %>%
  select(gnis_name = GNIS_Name,
         strm_order,
         reach_leng_m = reach_leng,
         chnk,
         chnk_use,
         sthd,
         sthd_use,
         rec_hist_c = S2_02_11,
         proj_2040 = S30_2040D,
         proj_2080 = S32_2080D) %>%
  # keep only reaches used by either sp/sum chinook or steelhead (according to StreamNet)
  filter(chnk == TRUE | sthd == TRUE)

# combinations of species/life stages and threshholds
spec_ls = c("chnk_spawn", "chnk_parr", "sthd_parr")
thresh = c("abv_optimum", "abv_max", "abv_acute")

# assign temp (c) values to each
temp_c_values = c(
  14.5, 17.7, 20, # chinook spawning
  16, 19, 22,     # chinook parr rearing
  18, 19, 22      # steelhead parr rearing
)

# create data frame of species/life stage temp threshhold values
thresh_df = expand.grid(spec_ls = spec_ls, thresh = thresh) %>%
  mutate(temp_c = temp_c_values)

# eck, change this to loop over populations instead, and then trim thresh_df to the appropriate rows depending on the set of
# populations being evaluated.

# results_df = NULL
# for (n in 1:nrow(thresh_df)) {
#
#   # grab the species/life stage and threshold
#   sl = thresh_df[n,] %>% pull(spec_ls) %>% as.character()
#   th = thresh_df[n,] %>% pull(thresh) %>% as.character()
#   dc = thresh_df[n,] %>% pull(temp_c)
#
#   # grab appropriate trt populations polygons
#   if (str_detect(sl, "chnk")) { pops = chnk_pops %>% select(popid = TRT_POPID) }
#   if (str_detect(sl, "sthd")) { pops = sthd_pops %>% select(popid = TRT_POPID) }
#
#   for (p in 1:nrow(pops)) {
#     pop_poly = pops[p,]
#     pop_tmp = srb_norwest_sf %>%
#       st_intersection() %>%
#       st_drop_geometry()
#
#   }
#
# }


