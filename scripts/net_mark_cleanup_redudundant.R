#CBE 11/26/24
# reading in and formatting Net Mark Rate data
# 
# Update: Colt already wrote a script for this, tidy_net_mark_rates.R.
# Results look the same, going to use that script.

library(tidyverse)
library(readxl)
library(janitor)
library(here)

read_section = function(range.use, year.set, class.set, week.range){
  dat = read_xlsx(here("original_data/Net Mark Rate for Creel 2021-2023_021624_DateLineUpFINAL.xlsx"), 
                  sheet = "EncountersByDate", range = range.use, col_names = FALSE)
  names(dat) = c("dates", "AD", "UM")
  weeks = read_xlsx(here("original_data/Net Mark Rate for Creel 2021-2023_021624_DateLineUpFINAL.xlsx"), 
                    sheet = "EncountersByDate", range = week.range, col_names = FALSE)
  dat$week = as.numeric(weeks[[1]])
  dat$year = year.set
  dat$class = class.set
  print(head(dat))
  return(dat)
}

res = rbind(
  read_section(range.use = "B2:D17", year.set = 2021, class.set = "R", week.range = "A2:A17"),
  read_section(range.use = "E2:G17", year.set = 2022, class.set = "R", week.range = "A2:A17"),
  read_section(range.use = "H2:J17", year.set = 2023, class.set = "R", week.range = "A2:A17"),
  
  read_section(range.use = "N2:P17", year.set = 2021, class.set = "K", week.range = "M2:M17"),
  read_section(range.use = "Q2:S17", year.set = 2022, class.set = "K", week.range = "M2:M17"),
  read_section(range.use = "T2:V17", year.set = 2023, class.set = "K", week.range = "M2:M17")
) |> 
  pivot_longer(cols = c("AD", "UM"), names_to = "mark.status")
