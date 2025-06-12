## benchmarking the time to catch for various species using test fishing data
library(pssp)
library(here)
library(tidyverse)
library(patchwork)

## Ty helped with this query
dat_test = pssp_query("-- this is the code for the materialized test fishing view

SELECT s.survey_datetime,
       s.start_datetime as survey_start,
       s.end_datetime as survey_end,
       se.encounter_datetime,
       se.encounter_number,
       s.survey_id,
       spl.sampling_program_code                                                                AS agency,
       cal.catch_area_code,
       crtl.catch_result_type_code,
       --s.start_datetime,
       --s.end_datetime,
      -- array_to_string(array_agg(DISTINCT concat(sa.first_name, ' ', sa.last_name)), ','::text) AS sampler,
       sl.common_name,
       -- array_to_string(array_agg(DISTINCT hll.hooking_location_description), ','::text)         AS hooking_locations,
       -- ppl.pinniped_predation_description,
       -- crtl.catch_result_type_code,
       acsl.adipose_clip_status_code,
       -- flm.length_measurement_centimeter,
       -- fgsl.fishing_gear_size_centimeter,
       -- fglt.fishing_gear_type_description,
       -- flmtl.length_type_description,
       --rhtl.health_type_short_description,
       l.latitude_decimal_degrees_wgs84,
       l.longitude_decimal_degrees_wgs84,
       -- i.genetic_sample_number,
       i.individual_fish_id,
       array_to_string(array_agg(DISTINCT tstl.target_species_type_description), ','::text)     AS target_species
       --s.comment_text                                                                           AS survey_comment,
       --se.comment_text                                                                          AS encounter_comment
FROM survey s
         JOIN survey_sampler ss ON s.survey_id = ss.survey_id
         JOIN sampler sa ON ss.sampler_id = sa.sampler_id
         JOIN survey_type_lut stl ON s.survey_type_id = stl.survey_type_id
         JOIN survey_event se ON s.survey_id = se.survey_id
         JOIN sampling_program_lut spl ON s.sampling_program_id = spl.sampling_program_id
         JOIN location l ON se.encounter_location_id = l.location_id
         JOIN catch_area_lut cal ON se.catch_area_id = cal.catch_area_id
         JOIN fish_encounter fe ON se.survey_event_id = fe.survey_event_id
         LEFT JOIN fishing_gear_size_lut fgsl ON fe.fishing_gear_size_id = fgsl.fishing_gear_size_id
         LEFT JOIN fishing_gear_type_lut fglt ON fe.fishing_gear_type_id = fglt.fishing_gear_type_id
         LEFT JOIN encounter_gear_type_lut egtl ON fe.encounter_gear_type_id = egtl.encounter_gear_type_id
         JOIN species_lut sl ON fe.species_id = sl.species_id
         JOIN adipose_clip_status_lut acsl ON fe.adipose_clip_status_id = acsl.adipose_clip_status_id
         JOIN catch_result_type_lut crtl ON fe.catch_result_type_id = crtl.catch_result_type_id
         LEFT JOIN target_species ts ON se.survey_event_id = ts.survey_event_id
         LEFT JOIN target_species_type_lut tstl ON ts.target_species_type_id = tstl.target_species_type_id
         LEFT JOIN hooking_location hl ON fe.fish_encounter_id = hl.fish_encounter_id
         LEFT JOIN hooking_location_lut hll ON hl.hooking_location_lut_id = hll.hooking_location_lut_id
         LEFT JOIN release_health_type_lut rhtl ON fe.release_health_type_id = rhtl.release_health_type_id
         LEFT JOIN pinniped_predation pp ON fe.fish_encounter_id = pp.fish_encounter_id
         LEFT JOIN pinniped_predation_lut ppl ON pp.pinniped_predation_lut_id = ppl.pinniped_predation_lut_id
         LEFT JOIN individual_fish i ON fe.fish_encounter_id = i.fish_encounter_id
         LEFT JOIN fish_length_measurement flm ON i.individual_fish_id = flm.individual_fish_id
         LEFT JOIN fish_length_measurement_type_lut flmtl
                   ON flm.fish_length_measurement_type_id = flmtl.fish_length_measurement_type_id
WHERE stl.survey_type_description::text = 'Puget Sound test fishing survey'::text
  AND se.void_encounter_indicator = false
GROUP BY s.survey_datetime, se.encounter_datetime, spl.sampling_program_code, cal.catch_area_code, s.start_datetime,
         s.end_datetime, sl.common_name, ppl.pinniped_predation_description, crtl.catch_result_type_code,
         acsl.adipose_clip_status_code, flm.length_measurement_centimeter, fgsl.fishing_gear_size_centimeter,
         fglt.fishing_gear_type_description, flmtl.length_type_description, rhtl.health_type_short_description,
         l.latitude_decimal_degrees_wgs84, l.longitude_decimal_degrees_wgs84, i.genetic_sample_number,
         i.individual_fish_id, s.comment_text, se.comment_text, se.encounter_number, s.survey_id
")

dat_test <- dat_test |> 
  ## SUPER IMPORTANT: There are DUPLICATE entries of fish because of multiple measurement types. Super annoying. 
  ## removing duplicates here.
  select(-encounter_number) |> 
  distinct()

write_csv(dat_test, here("cleaned_data/test-fishing-catch.csv"))
  

## Calculating time to catch --------------------------

## Need to give encounter number to those that are missing it -- order by datetime.
dat <- dat_test |> 
  ## SUPER IMPORTANT: There are DUPLICATE entries of fish because of multiple measurement types. Super annoying. 
  ## removing duplicates here.
  select(-encounter_number) |> 
  distinct() |> 
  group_by(survey_id) |> 
  arrange(encounter_datetime) |> 
  mutate(encounter_id = 1:n(),
         .after = encounter_datetime) |>
  mutate(min_since_last = (as.numeric(encounter_datetime) - as.numeric(lag(encounter_datetime)))/60) |> 
  ungroup() |> 
  mutate(year = year(survey_datetime),
         .before = survey_datetime) |> 
  select(-survey_datetime,
         -latitude_decimal_degrees_wgs84,
         -longitude_decimal_degrees_wgs84,
         -agency
  ) |> 
  filter(!is.na(min_since_last)) |> 
  select(year, catch_area_code, common_name, min_since_last, encounter_datetime, survey_id, target_species) |> 
  filter(common_name %in% c("Chinook Salmon", "Pink Salmon"),
         target_species == "Chinook salmon")

ggplot(dat, aes(x = min_since_last))+
  geom_density()+
  facet_wrap(.~ common_name)+
  xlim(c(0, 100))

ggplot(dat |> filter(min_since_last<=100), aes(x = min_since_last, col = common_name))+
  geom_density()+
  xlim(c(0, 100))+
  facet_wrap(.~ catch_area_code)+
  theme_classic()+
  labs(x = "Minutes to catch",
       title = "Test fishing time-to-catch")

## Empirical cdfs

temp = ecdf(dat |> filter(min_since_last<=500,
                          common_name == "Chinook Salmon") |> 
              pull(min_since_last))

temp(10)

temp = ecdf(dat |> filter(min_since_last<=500,
                          common_name == "Pink Salmon") |> 
              pull(min_since_last))
temp(10)

## It looks like the test fishing data has really fast catches. 40-55% of fish are caught within 10 minutes of the last fish, according to the empirical cdf functions. Not quite what we want.

## BUT. That's not really the relevant metric. Average time to catch weight by FISH is going to 
## be mostly full of really fast catches. We really want "If you spent an hour fishing, how many fish would you reasonably catch"
## E.g., cpue.

## Some problem entries here. Like negative durations. We'll add in some filters for that
## 
dat_cpue = dat_test |> 
  select(-encounter_number) |> 
  distinct() |> #View()
  group_by(survey_id, survey_start, survey_end, catch_area_code, target_species) |> 
  summarize(coho_count = sum(common_name == "Coho Salmon"),
            chinook_count = sum(common_name == "Chinook Salmon"),
            pink_count = sum(common_name == "Pink Salmon"),
            total_count = coho_count + chinook_count + pink_count) |> 
  ungroup() |> 
  mutate(survey_duration_hours = (as.numeric(survey_end)-as.numeric(survey_start)) / 60 / 60,
         rod_duration_hours = survey_duration * 2) |> 
  mutate(coho_cpue = coho_count/rod_duration_hours,
         chinook_cpue = chinook_count/rod_duration_hours,
         pink_cpue = pink_count/rod_duration_hours,
         total_cpue = total_count/rod_duration_hours) 

dat_test |> 
  select(-encounter_number) |> 
  distinct() |>
  filter(survey_id == "59876730-9ABB-429E-9273-12DF81838294") |> 
  write_csv(here("Example-problem-survey.csv"))

## plot coho
median_coho_cpue =
  dat_cpue |> 
  filter(target_species == "Coho salmon") |> 
  pull(coho_cpue) |> 
  median()

gp.coho = dat_cpue |>
  filter(target_species == "Coho salmon") |> 
  ggplot(aes(x = coho_cpue))+
    geom_histogram(bins = 100)+
  geom_vline(xintercept = median_coho_cpue, linetype = 2)+
  scale_x_continuous(breaks = (0:8)/2,
                     limits = c(0, 4))+
  labs(title = 'Coho test fishing CPUE',
       subtitle = "Coho-targeted trips only, dashed line = median",
       x = "cpue (catch per hour)")

## plot chinook
median_chinook_cpue =
  dat_cpue |> 
  filter(target_species == "Chinook salmon") |> 
  pull(chinook_cpue) |> 
  median()

gp.chin = dat_cpue |>
  filter(target_species == "Chinook salmon") |> 
  ggplot(aes(x = chinook_cpue))+
  geom_histogram(bins = 100)+
  geom_vline(xintercept = median_chinook_cpue, linetype = 2)+
  scale_x_continuous(breaks = (0:8)/2,
                     limits = c(0, 4))+
  labs(title = 'Chinook test fishing CPUE',
       subtitle = "Chinook-targeted trips only, dashed line = median",
       x = "cpue (catch per hour)")

## plot pink
median_pink_cpue =
  dat_cpue |> 
  filter(target_species == "Chinook salmon",
         pink_count>0) |> #View()
  pull(pink_cpue) |> 
  na.omit() |> 
  median()

gp.pink = dat_cpue |>
  filter(target_species == "Chinook salmon",
         pink_count>0) |> 
  ggplot(aes(x = pink_cpue))+
  geom_histogram(bins = 100)+
  geom_vline(xintercept = median_pink_cpue, linetype = 2)+
  scale_x_continuous(breaks = (0:8)/4,
                     limits = c(0, 4))+
  labs(title = 'Pink test fishing CPUE (only trips with 1+ pink catch)',
       subtitle = "Chinook-targeted trips only, dashed line = median",
       x = "cpue (catch per hour)")    

## plot pink
median_total_cpue =
  dat_cpue |> 
  filter(target_species == "Chinook salmon") |> #View()
  pull(total_cpue) |> 
  na.omit() |> 
  median()

gp.total = dat_cpue |>
  filter(target_species == "Chinook salmon") |> 
  ggplot(aes(x = total_cpue))+
  geom_histogram(bins = 100)+
  geom_vline(xintercept = median_total_cpue, linetype = 2)+
  scale_x_continuous(breaks = (0:8)/4,
                     limits = c(0, 4))+
  labs(title = 'Total test fishing CPUE',
       subtitle = "Chinook-targeted trips only, dashed line = median",
       x = "cpue (catch per hour)")  


gp.coho / gp.chin / gp.pink / gp.total
