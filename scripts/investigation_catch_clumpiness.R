## looking for autocorrelation in catch using test fishing data
## 
## Basic concern: if marked fish tend to hang together and unmarked fish tend to hang together and our model
## doesn't account for that, we will see more extreme interviews (tons of unmarked fish) than the model expects.
## 
## We can look for this kind of clumpiness in test fishing data, as we trust test fishing to correctly identify chinook and report UM vs non-UM. This is NOT going to be nisqually-specific results, but I would expect there to be clumpiness in other systems too, so this should be an okay first pass at least.

## The easiest way to get at patterns of clumpiness without trying to model yearly / seasonal patterns of marked and unmarked is to just look at the sequence of fish caught in individual surveys. If there's no clumpiness, then the order of catch should be random; if there's clumpiness, we're more likely to see strings of unmarked and then strings of marked(or vice versa). Since we know the total number of marked and unmarked caught in a given survey, we can compare the sequence against randomness. For a single survey this won't tell us much, but by looking at many surveys we can see if a pattern of clumpiness (string-yness?) emerges.



library(pssp)
library(tidyverse)
library(zoo)
library(doParallel)
library(foreach)
library(car)
library(lme4)

do.sim = FALSE


## Ty helped with this query
dat_test = pssp_query("-- this is the code for the materialized test fishing view

SELECT s.survey_datetime,
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
## Need to give encounter number to those that are missing it -- order by datetime.

dat <- dat_test |> 
  filter(common_name == "Chinook Salmon",
         adipose_clip_status_code %in% c("AD", "UM"),
         catch_result_type_code %in% c("R", "K")) |> 
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
  )

dat_survey_simplified = tibble(survey_id = unique(dat$survey_id)) |> 
  mutate(survey_id_simplified = seq_along(survey_id))

dat = left_join(dat, dat_survey_simplified, by = "survey_id") |> 
  relocate(survey_id_simplified, .before = survey_id) |> 
  select(-survey_id)

survey_summaries = dat |> 
  group_by(survey_id_simplified) |> 
  summarize(total_um = sum(adipose_clip_status_code == "UM"),
            total_ad = sum(adipose_clip_status_code == "AD"))


dat = left_join(dat, survey_summaries, by = "survey_id_simplified")

dat_robust = dat |> 
  filter(total_um > 3,
         total_ad > 3) 

##Okay, our data is ready!
dat_robust |> 
  filter(survey_id_simplified %in% sample(unique(dat_robust$survey_id_simplified), 50)) |> 
  mutate(survey_id_simplified = as.factor(survey_id_simplified)) |> 
  ggplot(aes(x = survey_id_simplified, y = encounter_id, fill = adipose_clip_status_code))+
  geom_tile()+
  theme_bw()+
  labs(x = "survey identity",
       y = "Chinook encounter in order",
       title = "looking for clumping of mark status (color) in each survey (vertical bar)")


## Checking that we're not looking across multiple areas in a single survey

dat_robust |> 
  select(survey_id_simplified, catch_area_code) |> 
  distinct() |> 
  pull(survey_id_simplified) |> 
  table() |> 
  table()

## looks good

## functions to help
## this takes two fish clip statuses and returns the transition type
find_transition = function(fin_pair){
  if(length(fin_pair) != 2){
    cli::cli_abort("need exactly two fish for transition")
  }
  case_when(
    fin_pair[1] == "UM" & fin_pair[2] == "UM" ~ "UM->UM",
    fin_pair[1] == "UM" & fin_pair[2] == "AD" ~ "UM->AD",
    fin_pair[1] == "AD" & fin_pair[2] == "UM" ~ "AD->UM",
    fin_pair[1] == "AD" & fin_pair[2] == "AD" ~ "AD->AD"
  )
}

find_transition_probs = function(survey_fins){
  #had some difficulties with the base probabilities, but I finally worked it out, I think
  #What is the frequency of each pair of fin types? With infinite sequence of fish we can use a simplification,
  #but for finite fish, we have a lower probability of observing doubles because we have used up one of the available clips in the first fish of the pair.
  #I think? Testing on a simple case seems to work well, but it's producing potentially biased results for bigger permutation sims.
  freq_um = sum(survey_fins == "UM")
  freq_ad = sum(survey_fins == "AD")
  transitions = rollapply(survey_fins, width = 2, FUN = find_transition)
  res = tibble(um_to_um = sum(transitions == "UM->UM"),
               ad_to_um = sum(transitions == "AD->UM"),
               um_to_ad = sum(transitions == "UM->AD"),
               ad_to_ad = sum(transitions == "AD->AD"),
               um_to_um_pred = freq_um * (freq_um - 1) / length(survey_fins), #subtracting kronecker delta
               ad_to_um_pred = freq_ad * (freq_um - 0) / length(survey_fins), #subtracting kronecker delta
               um_to_ad_pred = freq_um * (freq_ad - 0) / length(survey_fins), #subtracting kronecker delta
               ad_to_ad_pred = freq_ad * (freq_ad - 1) / length(survey_fins), #subtracting kronecker delta
  )
  ## the following isn't doing the normalization right, as my sims below identified.
  ## The um_to_um transition is going to be far more common that ad_to_um if um is more common. 
  res$um_to_um_diff = res$um_to_um - res$um_to_um_pred
  res$ad_to_um_diff = res$ad_to_um - res$ad_to_um_pred
  res$um_to_ad_diff = res$um_to_ad - res$um_to_ad_pred
  res$ad_to_ad_diff = res$ad_to_ad - res$ad_to_ad_pred
  return(res)
}

## Quick test: this should produce values of ~ 0:--------------------------------------------

nsim = 1000
um_to_um_null = numeric(nsim)
res.list = list()
for(i in 1:nsim){
  temp = sample(c(rep("UM", 5), rep("AD", 6)))
  res.temp = find_transition_probs(temp)
  res.list[[i]] = res.temp
  um_to_um_null[i] = res.temp$um_to_um_diff
}
mean(um_to_um_null)
sim.df = do.call(rbind, res.list)
View()

## Nope. Super consistently NOT doing what we want.



matrixify_transitions = function(transition_probs){
  res = matrix(c(transition_probs$um_to_um, transition_probs$um_to_ad,
                 transition_probs$ad_to_um, transition_probs$ad_to_ad),
               nrow = 2,
               ncol = 2,
               byrow = TRUE,
               dimnames = list(rownames = c("from_um", "from_ad"), 
                               colnames = c("to_um", "to_ad")))
  as.data.frame(res)
}


temp = dat_robust |> 
  filter(survey_id_simplified == dat_robust$survey_id_simplified[1]) |> 
  pull(adipose_clip_status_code)

# table(rollapply(temp, width = 2, FUN = find_transition))/(length(temp)-1)
find_transition_probs(temp)
matrixify_transitions(find_transition_probs(temp))
# matrixify_transitions(find_transition_probs(temp)) |> 
# gt(rownames_to_stub = TRUE)

survey_transitions = dat_robust |> 
  arrange(survey_id_simplified, survey_id_simplified) |> 
  nest(.by = c(year, survey_id_simplified)) |> 
  mutate(transitions = purrr::map_df(data,
                                     function(df){find_transition_probs(df$adipose_clip_status_code)})) |> 
  select(-data) |> 
  unnest(transitions)

## Summary
obs_probs = survey_transitions |> 
  summarize(um_to_um_diff = mean(um_to_um_diff),
            ad_to_um_diff = mean(ad_to_um_diff),
            um_to_ad_diff = mean(um_to_ad_diff),
            ad_to_ad_diff = mean(ad_to_ad_diff)) |> 
  ungroup() 


ggplot(survey_transitions,
       aes(x = um_to_um_diff))+
  geom_histogram(bins = 50)+
  geom_vline(xintercept = median(survey_transitions$um_to_um_diff))+
  theme_bw()+
  labs(x = "deviation from expected",
       y = "survey count",
       title = "Unmarked to unmarked transitions")


survey_transitions |> 
  group_by(year) |> 
  summarize(um_to_um_diff = mean(um_to_um_diff),
            ad_to_um_diff = mean(ad_to_um_diff),
            um_to_ad_diff = mean(um_to_ad_diff),
            ad_to_ad_diff = mean(ad_to_ad_diff)) |> 
  ungroup() 

#Huh. Might need to check the math on this. 

# Quick and dirty check: permutation test --------------------------------
# What is the distribution of values when we randomly reorder the fish?

if(do.sim){
  cl <- makeCluster(6)
  registerDoParallel(cl)
  nsim = 1000
  sim.ls = foreach(i = 1:nsim,
                   .packages = c("tidyverse", "zoo")) %dopar% {
                     sim.ls[[i]] = dat_robust |> 
                       arrange(survey_id_simplified, survey_id_simplified) |> 
                       nest(.by = c(year, survey_id_simplified)) |> 
                       mutate(transitions = purrr::map_df(data,
                                                          function(df){find_transition_probs(sample(df$adipose_clip_status_code))})) |> 
                       select(-data) |> 
                       unnest(transitions) |> 
                       summarize(um_to_um_diff = mean(um_to_um_diff),
                                 ad_to_um_diff = mean(ad_to_um_diff),
                                 um_to_ad_diff = mean(um_to_ad_diff),
                                 ad_to_ad_diff = mean(ad_to_ad_diff))
                   }
  stopCluster(cl)
  
  sim.probs = do.call(rbind, sim.ls)
  write_csv(sim.probs, 
            here("results/simulations/clumpiness-markov-null-sims.csv"))
} else {
  sim.probs = read_csv(here("results/simulations/clumpiness-markov-null-sims.csv"))
}
quantile(sim.probs$um_to_um_diff, probs = c(0.1, 0.9))
## same sort of problem.
## Is the issue the overlap situation?


## NEW approach: just use binomial regression and lag to look for serial correlation ------------------------------

calc_lagged_predictive_power = function(survey_fins){
  dat = tibble(fish_um = survey_fins == "UM") |> 
    mutate(lagged_um = lag(fish_um))
  out = glm(fish_um ~ lagged_um, data = dat, family = "binomial")
  res = data.frame(coef = coef(out)[2],
                   p = Anova(out)$`Pr(>Chisq)`)
  return(res)
}


survey_predictive = dat_robust |> 
  arrange(survey_id_simplified, encounter_id) |> 
  nest(.by = c(year, survey_id_simplified)) |> 
  mutate(logistic_results = purrr::map_df(data,
                                          function(df){calc_lagged_predictive_power(df$adipose_clip_status_code)})) |> 
  select(-data) |> 
  unnest(logistic_results)

hist(survey_predictive$coef, breaks = 50)

predictive_summary = data.frame(t(quantile(survey_predictive$coef, probs = c(0.025, 0.1, 0.5, 0.9, 0.975))))
predictive_summary$freq_significant = mean(survey_predictive$p < 0.05)
predictive_summary

## Simulations to check null


if(do.sim){
  cl <- makeCluster(6)
  registerDoParallel(cl)
  nsim = 1000
  
  sim.ls = foreach(i = 1:nsim, 
                   .packages = c("tidyverse", "lme4", "car")) %dopar% {
    
    survey_predictive = dat_robust |> 
      arrange(survey_id_simplified, encounter_id) |> 
      nest(.by = c(year, survey_id_simplified)) |> 
      mutate(logistic_results = purrr::map_df(data,
                                              function(df){calc_lagged_predictive_power(sample(df$adipose_clip_status_code))})) |> 
      select(-data) |> 
      unnest(logistic_results)
    
    cur.predictive_summary = data.frame(t(quantile(survey_predictive$coef, probs = c(0.025, 0.1, 0.5, 0.9, 0.975))))
    cur.predictive_summary$freq_significant = mean(survey_predictive$p < 0.05)
    
    sim.ls[[i]] = cur.predictive_summary
  }
  stopCluster(cl)
  
  sim.df = do.call(rbind, sim.ls)
  
  write_csv(sim.df, 
            here("results/simulations/clumpiness-logistic-null-sims.csv"))
} else {
  sim.df = read_csv(here("results/simulations/clumpiness-logistic-null-sims.csv"))
}

ggplot(sim.df, aes(x = X50.))+
  geom_histogram(bins = 50)+
  geom_vline(xintercept = predictive_summary$X50.)+
  geom_vline(xintercept = quantile(sim.df$X50., probs = c(0.025, 0.975)), linetype = 2)+
  theme_bw()+
  labs(xlab = "median")
