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

fishery.ls = get_fishery_data(fishery = "Nisqually salmon", years = 2021:2023)


interviews <- fishery.ls$interview |>  
  mutate(fishing_duration_minutes = (as.numeric(fishing_end_time) - as.numeric(fishing_start_time))/60,
         angler_minutes = fishing_duration_minutes * angler_count)
cli::cli_alert("Integrating `angler type` into `boat_used` and cutting out angler_type")
interviews$boat_used[is.na(interviews$boat_used) & interviews$angler_type == "Boat"] = "Yes"
interviews$boat_used[is.na(interviews$boat_used) & interviews$angler_type == "Bank"] = "No"

# bind date and waterbody data to the creel interview-based catch records
catch <- fishery.ls$catch |> 
  left_join(interviews |> 
              select(interview_id, event_date, water_body, year, month, week, fishing_duration_minutes, trip_guided, boat_type, boat_used,
                     angler_count, angler_minutes, trip_status, fishing_start_time, fishing_end_time),
            by = "interview_id") |> 
  filter(species == "Pink")
## integrating our two measures of when boats are used

write_csv(catch,
          here("cleaned_data/key_dataframes/creel_interview_catch_pinks.csv"))

## we have separate entries by fork length for measured catches. We don't want that for this.
catch = catch |> 
  group_by(interview_id, species, fate,
           event_date, water_body, fishing_duration_minutes, angler_count, year, month, week, trip_guided, boat_type, boat_used, trip_status, fishing_start_time, fishing_end_time, angler_minutes) |> 
  summarize(fish_count = sum(fish_count)) |> 
  ungroup()

## Create dataframe for chinook-only with all relevant 0s. 

df.dummy = expand_grid(interviews |> 
                         select(interview_id, event_date, water_body, fishing_duration_minutes, angler_count,
                                angler_minutes,year, month, week, trip_guided, boat_type, boat_used, trip_status,
                                fishing_start_time, fishing_end_time, angler_minutes) |> 
                         filter(!is.na(interview_id)),
                       species = "Pink",
                       fate = c("Kept", "Released")) 

catch.zerod = catch |> 
  filter(!is.na(interview_id)) |> 
  filter(species == "Pink")


catch.zerod = catch.zerod |> 
  full_join(df.dummy) |> 
  mutate(fish_count = replace_na(fish_count, 0),
         fin_mark = NA_real_)
## check uniques
table(table(catch.zerod$interview_id)) 
## all good!
# interview.check = catch.zerod |>
#   count(interview_id) |>
#   filter(n>8) |>
#   pull(interview_id)
# catch.zerod |>
#   filter(interview_id %in% interview.check) |>
#   View()
write_csv(catch.zerod,
          here("cleaned_data/key_dataframes/creel_interview_catch_pinks_withzeros.csv"))


