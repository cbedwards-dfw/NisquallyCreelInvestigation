#Colt Holley 12/10/2024
#Importing Nisqually tribal net data and reformatting to "tidy" format

library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(glue)

#import original data from Excel file
data_raw <- readxl::read_xlsx(path = "./original_data/20 years Net Timing Nisqually Clipped Unclipped Chinook.xlsx", sheet = 1, range = "A2:AK24")
data_raw <- janitor::clean_names(data_raw)

#initialize data_processing intermediary
data_processing <- data_raw

weeks <- c(29:46)

#trim first row
data_trim <- data_processing |> dplyr::slice(-1)

new_colnames <- c("year", rep(weeks, each = 2))
colnames(data_trim) <- new_colnames

#extract fin_mark
orig_marks <- as.character(as.vector(data_processing[1,2:37]))

#rename columns to include fin_mark as a suffix
colnames(data_trim)[2:37] <- paste0(colnames(data_trim)[2:37], "_", orig_marks)

#display new colnames
colnames(data_trim)

#pivot to long format
data_pivot <- data_trim |> 
  tidyr::pivot_longer(
    cols = -year,
    names_to = "week_and_mark",
    values_to = "catch"
  ) |> 
  separate(
    week_and_mark,
    into = c("week", "fin_mark"),
    sep = "_"
  )

#align fin_mark with creel data format
data_pivot <- data_pivot |> 
  mutate(
    fin_mark = case_when(
      fin_mark == "Clipped" ~ "AD",
      fin_mark == "Unclipped" ~ "UM"
    )
  )

#replace NAs with zero
# leave original version (including NAs) available for timeseries analysis
# add in special designation for the three weeks that were drift gill nets.
data_final <- data_pivot |> 
  dplyr::mutate(catch_original = catch,
                catch = ifelse(
                  is.na(catch), 0, catch
                ))
## Per convo with Craig: week 37 of 2023 and 2024 and week 38 of 2023 were drift gill nets, and should be viewed a bit differently.
data_insert = 
  tribble(~year, ~week, ~notes,
          2023, "37", "drift gill net",
          2023, "38", "drift gill net", 
          2024, "37", "drift gill net")
data_final = data_final |> 
  left_join(data_insert, by = c("year", "week"))

#write out formatted data frame
readr::write_csv(data_final, file = "./cleaned_data/tidy_20-years-net-mark-rates.csv")
