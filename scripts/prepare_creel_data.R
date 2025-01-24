##################################################
## Collin Edwards
## Thu Jan  2 12:27:22 2025
## Code to create our key dataframes: the `catch` (raw creel data) and `combined` (bss results)
##################################################



library("CreelEstimateR")
library("tidyverse")
library("here")

## Creel interview data -----------------


# define fisheries we want data for
fisheries <- c("Nisqually salmon 2021", "Nisqually salmon 2022", "Nisqually salmon 2023")

# download the data from data.wa.gov
datapull <- set_names(fisheries) |> 
  map(~fetch_dwg(fishery_name = .x))

interviews <- datapull$`Nisqually salmon 2023`$interview |> 
  bind_rows(datapull$`Nisqually salmon 2022`$interview, datapull$`Nisqually salmon 2021`$interview) |> 
  mutate(fishing_duration_minutes = (fishing_end_time - fishing_start_time)/60)

# bind date and waterbody data to the creel interview-based catch records
catch <- datapull$`Nisqually salmon 2023`$catch |> 
  bind_rows(datapull$`Nisqually salmon 2022`$catch, datapull$`Nisqually salmon 2021`$catch) |>
  left_join(interviews |> 
              select(interview_id, event_date, water_body, fishing_duration_minutes),
            by = "interview_id") |> 
  mutate(
    year = year(event_date),
    month = month(event_date),
    management_week = framrsquared::management_week(event_date)
  )

write_csv(catch,
          here("cleaned_data/key_dataframes/creel_interview_catch.csv"))

## Combine BSS results from Evan's runs-------------


# catch estimates calculated for total encounters of ALL adult Chinook
estimates_total_encounters <- readr::read_csv(here("cleaned_data/creel_estimates", "model_estimates_stratum_s1s2_total_encounters.csv"))

# catch estimates calculated for total encounters of only AD-clipped (marked) adult Chinook
estimates_ad_encounters <- readr::read_csv(here("cleaned_data/creel_estimates", "model_estimates_stratum_ad_encounters.csv"))

# catch estimates calculated for total encounters of only UM (unmarked) released adult Chinook
cli::cli_alert_danger("Limitation with BSS data loading: Evan code loads in UM released, but that file is not on the repo. When available, uncomment code to read in.")
# estimates_UM_released <- readr::read_csv(here("cleaned_data/creel_estimates", "model_estimates_stratum_um_released.csv"))

# bind together and filter to outputs generated with the Bayesian state space model 
cli::cli_alert_danger("Limitation with BSS data loading: Evan code loads in UM released, but that file is not on the repo. When available, uncomment code to bind together.")
# combined <- estimates_total_encounters |> rbind(estimates_ad_encounters, estimates_UM_released) |>
combined <- estimates_total_encounters |> rbind(estimates_ad_encounters) |>
  filter(model_type == "BSS" & estimate_category == "catch") |>  
  select(event_date = min_event_date, section_num, period, day_type, angler_final, est_cg,
         estimate_category, estimate_type, estimate_value) |> 
  dplyr::mutate(
    management_week = framrsquared::management_week(event_date)
  ) |> 
  pivot_wider(names_from = estimate_type, values_from = estimate_value)

write_csv(combined,
          here("cleaned_data/key_dataframes/bss_combined.csv"))

