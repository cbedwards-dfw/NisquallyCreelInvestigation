## Ty pointed out that when folks catch a CWT fish, they report the location of that fish. We can filter
## our data to surveys in which a CWT fish was caught in a location around the nisqually river mouth
## 
## This could give us a general reference point. Probably there is not enough data, but we can see
## 
## Ty suggested talking to Samantha Bundt about locations -- there may be more than what I listed here.

library(pssp)
library(tidyverse)
location_codes = c(132324,
                   132323,
                   132279,
                   1576
)

vec_maker = function(vec){
  paste()
}

catch_near_nisqually <- pssp_query("
SELECT
    s.survey_datetime,
    extract(YEAR from s.survey_datetime) as year,
    catch_area_code,
    sl.common_name,
    se.survey_event_id,
    acsl.adipose_clip_status_code,
    catch_result_type_lut.catch_result_type_code,
    legal_size_status_lut.legal_size_status_description,
    fe.fish_count
FROM
    survey s
JOIN survey_event se on s.survey_id = se.survey_id
JOIN fish_encounter fe on se.survey_event_id = fe.survey_event_id
JOIN species_lut sl on fe.species_id = sl.species_id
JOIN catch_result_type_lut on fe.catch_result_type_id = catch_result_type_lut.catch_result_type_id
JOIN catch_area_lut on se.catch_area_id = catch_area_lut.catch_area_id
JOIN adipose_clip_status_lut acsl on fe.adipose_clip_status_id = acsl.adipose_clip_status_id
LEFT JOIN legal_size_status_lut on fe.legal_size_status_id = legal_size_status_lut.legal_size_status_id
WHERE se.survey_event_id IN  (
    SELECT
    se.survey_event_id
FROM
    survey s
JOIN survey_event se on s.survey_id = se.survey_id
JOIN fish_encounter fe on se.survey_event_id = fe.survey_event_id
JOIN species_lut sl on fe.species_id = sl.species_id
JOIN catch_result_type_lut on fe.catch_result_type_id = catch_result_type_lut.catch_result_type_id
JOIN catch_area_lut on se.catch_area_id = catch_area_lut.catch_area_id
JOIN individual_fish indf on fe.fish_encounter_id = indf.fish_encounter_id
JOIN location l on fe.fish_location_id = l.location_id
WHERE l.location_code IN('132324', '132323', '132279', '1576')
    )
")

catch_locations <- pssp_query("
SELECT
se.survey_event_id,
l.location_name
FROM
survey s
JOIN survey_event se on s.survey_id = se.survey_id
JOIN fish_encounter fe on se.survey_event_id = fe.survey_event_id
JOIN species_lut sl on fe.species_id = sl.species_id
JOIN catch_result_type_lut on fe.catch_result_type_id = catch_result_type_lut.catch_result_type_id
JOIN catch_area_lut on se.catch_area_id = catch_area_lut.catch_area_id
JOIN individual_fish indf on fe.fish_encounter_id = indf.fish_encounter_id
JOIN location l on fe.fish_location_id = l.location_id
WHERE l.location_code IN('132324', '132323', '132279', '1576')
")
