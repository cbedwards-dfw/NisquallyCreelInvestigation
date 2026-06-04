#Colt H 11/20/2024
#import tribal net catch data for Nisqually 2021-2023

#original file "Net Mark Rate for Creel 2021-2023_021624_DateLineUpFINAL.xlsx"

library(tidyverse)
library(readxl)
library(janitor)

# load data ####

data_raw <- readxl::read_xlsx("./original_data/Net Mark Rate for Creel 2021-2023_021624_DateLineUpFINAL.xlsx", sheet = 2)

#first coarse formatting of column names
data_processing <- data_raw |> janitor::clean_names()
colnames(data_processing)

#drop bottom table of total encounters, keep 2021-23 AD/UM released and 2021-23 AD/UM kept tables to format
data_processing <- data_processing |> 
  dplyr::slice_head(n = 16) |>  #top rows
  dplyr::select(-x11, -x12)  #drop two empty spacing columns

#separate tables - RELEASED ####
#take released table, split by year, reformat and recombine
released <- data_processing |> 
  dplyr::select(x1:x2023_um_r)#first 10 columns

#21
released_2021 <- released |> 
  dplyr::select(1:4)|> #col 1 has stat week, cols 2-4 have 2021 data
  dplyr::mutate(year = 2021) |> 
  dplyr::relocate(year) |> 
  dplyr::rename(
    stat_week = x1,
    day_interval = x2021_2,
    ad = x2021_ad_r,
    um = x2021_um_r
  )

#22
released_2022 <- released |> 
  dplyr::select(5:7) |> #2022 data
  dplyr::mutate(year = 2022,
                stat_week = as.character(seq(31,46))) |> #add stat weeks from column 1
  dplyr::relocate(year, stat_week) |> 
  dplyr::rename(
    day_interval = x2022_5,
    ad = x2022_ad_r,
    um = x2022_um_r
  )

released_2022$um <- as.numeric(released_2022$um)

#23
released_2023 <- released |> 
  dplyr::select(8:10) |>  #2023 data
  dplyr::mutate(year = 2023,
                stat_week = as.character(seq(31,46))) |> 
  dplyr::relocate(year, stat_week) |> 
  dplyr::rename(
    day_interval = x2023_8,
    ad = x2023_ad_r,
    um = x2023_um_r
  )

released_2023$ad <- as.numeric(released_2023$ad)
released_2023$um <- as.numeric(released_2023$um)

#recombine released table
released_post <- dplyr::bind_rows(released_2021, released_2022, released_2023) |> 
  dplyr::mutate(fate = "released")

released_post <- released_post |> 
  tidyr::pivot_longer(cols = c(ad, um), names_to = "fin_mark", values_to = "catch")

#separate tables - KEPT ####
#take kapt table, split by year, reformat and recombine
kept <- data_processing |> 
  dplyr::select(x13:x2023_um_k) #second 10 columns

#21
kept_2021 <- kept |> 
  dplyr::select(1:4)|> #col 1 has stat week, cols 2-4 have 2021 data
  dplyr::mutate(year = 2021) |> 
  dplyr::relocate(year) |> 
  dplyr::rename(
    stat_week = x13,
    day_interval = x2021_14,
    ad = x2021_ad_k,
    um = x2021_um_k
  )

kept_2021$ad <- as.numeric(kept_2021$ad)
kept_2021$um <- as.numeric(kept_2021$um)

#22
kept_2022 <- kept |> 
  dplyr::select(5:7) |>  #2022 data
  dplyr::mutate(year = 2022,
                stat_week = as.character(seq(31,46))) |> #add stat weeks from column 1
  dplyr::relocate(year, stat_week) |> 
  dplyr::rename(
    day_interval = x2022_17,
    ad = x2022_ad_k,
    um = x2022_um_k
  )

kept_2022$ad <- as.numeric(kept_2022$ad)
kept_2022$um <- as.numeric(kept_2022$um)

#23
kept_2023 <- kept |> 
  dplyr::select(8:10) |> #2023 data
  dplyr::mutate(year = 2023,
                stat_week = as.character(seq(31,46))) |> 
  dplyr::relocate(year, stat_week) |> 
  dplyr::rename(
    day_interval = x2023_20,
    ad = x2023_ad_k,
    um = x2023_um_k
  )

kept_2022$ad <- as.numeric(kept_2022$ad)
kept_2023$um <- as.numeric(kept_2023$um)

#recombine kept table
kept_post <- dplyr::bind_rows(kept_2021, kept_2022, kept_2023) |> 
  dplyr::mutate(fate = "kept")

kept_post <- kept_post |> 
  tidyr::pivot_longer(cols = c(ad, um), names_to = "fin_mark", values_to = "catch")

#recombine formatted tables ####
data_final_output <- dplyr::bind_rows(released_post, kept_post) |> 
  dplyr::mutate(catch = round(catch, 1))

# save results ####
readr::write_csv(data_final_output, "./cleaned_data/tidy_net_mark_rates.csv")
