## Ty pointed out that when folks catch a CWT fish, they report the location of that fish. We can filter
## our data to surveys in which a CWT fish was caught in a location around the nisqually river mouth
## 
## This could give us a general reference point. Probably there is not enough data, but we can see
## 
## I used CWT lat lons and wrote a shiny app to look at cwt locations on a map
## The following locations are very close to nisqually
## location_code	location_name	longitude	latitude
# 132011	ANDERSON ISLAND	-122.7068	47.12675
# 132276	LUHR BEACH DOCK	-122.7267	47.10038
# 132279	LYLE POINT	-122.7267	47.10040
# 132324	NISQUALLY OFF MOUTH	-122.7011	47.11345
# 132323	NISQUALLY REACH	-122.7009	47.10844
# 132348	ORO BAY	-122.6918	47.13780
## 

library(pssp)
library(tidyverse)
## The following still must be copied in to the query language!
location_codes = c(132011,
                   132276,
                   132279,
                   132324,
                   132323,
                   132348
)


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
WHERE l.location_code IN('132011', '132276', '132279', '132324', '132323', '132348')
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
WHERE l.location_code IN('132011', '132276', '132279', '132324', '132323', '132348')
")

dim(catch_near_nisqually)

## catch in the correct years
catch_near_nisqually |> filter(year %in% 2021:2023) |> dim()