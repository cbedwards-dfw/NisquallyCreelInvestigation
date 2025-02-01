##################################################
## Collin Edwards
## Fri Jan 10 11:45:18 2025
## Preliminary look at prestige bias
##################################################

library(here)
library(tidyverse)
library(lme4)
library(mgcv)
library(patchwork)
library(gratia)
library(DHARMa)

doy_2md=function(i){
  ymd=as.Date(i-1, origin="2019-01-01")
  return(format(ymd, "%b %d"))
}

## rerun the following to recreate the clean data
if(FALSE){
  source(here("scripts/prepare_creel_data.R"))
}

raw = read_csv(here("cleaned_data/key_dataframes/creel_interview_catch_withzeros.csv")) 
## Note: problem with col 4 being interpreted as a T/F instead of char

dat = raw |> 
  filter(species == "Chinook")

dat.sum = dat |> 
  filter(fin_mark %in% c("UM", "AD")) |> 
  group_by(interview_id, 
           life_stage, event_date, year, month, management_week, fishing_duration_minutes) |> 
  summarize(um = sum(fish_count[fin_mark == "UM"]),
            ad = sum(fish_count[fin_mark == "AD"]),
            released = sum(fish_count[fate == "Released"]),
            kept = sum(fish_count[fate == "Kept"]),
  ) |> 
  ungroup() |> 
  mutate(had.ad = ad>0,
         had.um = um>0,
         had.release = released>0,
         had.kept = kept >0)

## for now, ignore jacks
dat.sum = dat.sum |> 
  filter(life_stage == "Adult")

## note: should ground truth that!

## simplest approach: 

facet_names <- c(`FALSE` = "Reported no kept fish",
                 `TRUE` = "Reported 1+ kept fish")

dat.sum |> 
  ggplot(aes(x = um, fill = had.kept)) +
  geom_histogram() +
  facet_wrap(.~ had.kept,
             labeller = as_labeller(facet_names)) + 
  labs(x = "# reported unmarked chinook",
       y = "# of interviews",
       title = "Apparent prestige bias")+
  theme_bw(base_size = 14)+
  theme(legend.position = "none")


dat.fit = dat |> 
  filter(life_stage == "Adult") |> 
  filter(fin_mark %in% c("UM", "AD")) |> 
  group_by(interview_id, fin_mark, event_date, year, month, fishing_duration_minutes) |>   
  summarize(fish_count = sum(fish_count)) |> 
  ungroup() |> 
  ## need to collapse the data down a bit
  left_join(dat.sum |> 
              select(interview_id, had.ad, had.um, had.release, had.kept),
            by = "interview_id") |> 
  mutate(doy = yday(event_date),
         fin_mark = as.factor(fin_mark),
         had.ad = as.factor(had.ad),
         had.um = as.factor(had.um),
         had.release = as.factor(had.release),
         had.kept = as.factor(had.kept),
         yearfac = as.factor(year)) |> 
  filter(!is.na(fishing_duration_minutes)) |> 
  mutate(round_cat1 = case_when(
    fish_count >= 3 & fish_count <= 7 ~ "3-7",
    fish_count >= 8 & fish_count <= 12 ~ "8-12",
    fish_count >= 13 ~ "13+",
  )) |> 
  mutate(round_cat1 = factor(round_cat1, levels = c("3-7", "8-12", "13+"))) |> 
  mutate(round_cat2 = case_when(
    fish_count >= 3 & fish_count <= 7 ~ "3-7",
    fish_count >= 8 ~ "8+"
  )) |> 
  mutate(round_cat2 = factor(round_cat2, levels = c("3-7", "8+")))

cli::cli_alert("Reminder: skipping observations with no reported fishing time. These are ~ 500 out of 63,500, so not super worried about it, but in theory we could replace those with the average time. Consult with Evan.")

## want to also have time on the water

## consider adding random effect of interview!
 
## GAM model -------------------------------------------

out = gam(fish_count ~ fin_mark + s(doy, by = fin_mark) + s(yearfac, bs = 're') + fishing_duration_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)

# draw(out)

gp.seasonality = conditional_values(
  out,
  condition = c("doy", "fin_mark")
) |> 
  draw()+
  scale_x_continuous(limits = c(200, 300),
                     labels = doy_2md)+
  scale_y_continuous(limits = c(0, .5))+
  labs(x = "",
       y = "Predicted encounters per hour",
       title = "Seasonality of encounters")+
  theme_bw(base_size = 14)

gp.seasonality

ggsave(filename = here("figures/catch-model-results/negbin-season-averages.pdf"),
      plot = gp.seasonality,
      width = 8, height = 5)


if(FALSE){
  simulationOutput <- simulateResiduals(fittedModel = out, plot = F)
  plot(simulationOutput)
}


dat.fit$fish_count_predicted = as.numeric(predict(out, newdata = dat.fit, type = "response"))
dat.fit = dat.fit |>
  mutate(fish_count_predicted_int = round(fish_count_predicted))


## Looking at predicted vs observed distributions of gam model ==================

## Returning to our simple gam model: 

mus = predict(out, newdata = dat.fit, type = "response")
cur.theta = out$family$getTheta(TRUE)

# dnbinom(0:10, size = cur.theta, mu = mus[1])

foo <- function(x){dnbinom(0:30, size = cur.theta, mu = x)}
df = data.frame(mu = mus)|> 
  mutate(mat = do.call(rbind, purrr::map(mu, foo)))

df.pred = as.data.frame(df$mat)
#gros names for a sec:
names(df.pred) = 0:30  
## now combine with the data and use pivoting to put into handy form
df.pred <- dat.fit |> 
  select(-fish_count_predicted, -fish_count_predicted_int) |> 
  cbind(df.pred) |> 
  pivot_longer(cols = "0":"30",
               names_to = "predicted_count",
               values_to = "predicted_probability") |> 
  mutate(predicted_count = as.numeric(predicted_count))

## With this dataframe, we can now sum up the number of counts we would expect to see
## for marked and unmarked fish, *accounting* for the variability in negative binomials

freq.pred = df.pred |> 
  group_by(fin_mark, predicted_count) |> 
  summarize(expected_frequency = sum(predicted_probability)) |> 
  ungroup() |> 
  ## adding rounding categorizations
  mutate(round_cat1 = case_when(
    predicted_count >= 3 & predicted_count <= 7 ~ "3-7",
    predicted_count >= 8 & predicted_count <= 12 ~ "8-12",
    predicted_count >= 13 ~ "13+",
  )) |> 
  mutate(round_cat1 = factor(round_cat1, levels = c("3-7", "8-12", "13+"))) |> 
  mutate(round_cat2 = case_when(
    predicted_count >= 3 & predicted_count <= 7 ~ "3-7",
    predicted_count >= 8 ~ "8+"
  )) |> 
  mutate(round_cat2 = factor(round_cat2, levels = c("3-7", "8+")))

## basic plotting ================================

gp.fit = dat.fit |> 
  filter(fin_mark == "AD") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(-1,11))+
  ggtitle("Observed counts of AD fish")+
  theme_bw(base_size = 14)

gp.pred = freq.pred |> 
  filter(fin_mark == "AD") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of AD fish")+
  xlim(c(-1,11))+
  theme_bw(base_size = 14)

gp.cur = gp.fit / gp.pred & plot_annotation(title = "Negative binomial model",
                                            subtitle = "from `preliminary-catch-model.R`")

ggsave(filename = here("figures/catch-model-results/total-AD.pdf"),
       plot = gp.cur,
       height = 8, width = 8
)


pred.partial = freq.pred |> 
  filter(fin_mark == "AD") |> 
  group_by(predicted_count) |> 
  summarize(expected_frequency = sum(expected_frequency)) |> 
  ungroup() |> 
  mutate(scenario = "prediction") |> 
  rename(fish_count = predicted_count, frequency = expected_frequency)

obs.partial = dat.fit |> 
  filter(fin_mark == "AD") |> 
  count(fish_count) |>
  rename(frequency = n) |>
  ##fill in 0s
  full_join(expand_grid(fish_count = seq(0, max(dat.fit$fish_count), by = 1)
  )) |> 
  mutate(frequency = if_else(is.na(frequency),
                             0,
                             frequency)) |> 
  mutate(scenario = "observation")

gp.cur = bind_rows(pred.partial, obs.partial) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("AD catch frequencies")+
  labs(subtitle = "from `preliminary-catch-model.R`, using negbin model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/total-AD-v2.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)


gp.cur = bind_rows(pred.partial, obs.partial) |> 
  filter(fish_count > 0) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("AD catch frequencies (skipping 0s to provide more clarity)")+
  labs(subtitle = "from `preliminary-catch-model.R`, using negbin model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/total-AD-v2-zoom.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)



gp.cur = gp.fit / gp.pred & plot_annotation(title = "Negative binomial model",
                                            subtitle = "from `preliminary-catch-model.R`")




gp.fit = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(-1,16))+
  ggtitle("Observed counts of UM fish")+
  theme_bw(base_size = 14)

gp.pred = freq.pred |> 
  filter(fin_mark == "UM") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of UM fish")+
  xlim(c(-1,16))+
  theme_bw(base_size = 14)



ggsave(filename = here("figures/catch-model-results/total-UM.pdf"),
       plot = gp.fit / gp.pred & plot_annotation(title = "Negative binomial model",
                                                 subtitle = "from `preliminary-catch-model.R`"),
       height = 8, width = 8
)



pred.partial = freq.pred |> 
  filter(fin_mark == "UM") |> 
  group_by(predicted_count) |> 
  summarize(expected_frequency = sum(expected_frequency)) |> 
  ungroup() |> 
  mutate(scenario = "prediction") |> 
  rename(fish_count = predicted_count, frequency = expected_frequency)

obs.partial = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |>
  rename(frequency = n) |>
  ##fill in 0s
  full_join(expand_grid(fish_count = seq(0, max(dat.fit$fish_count), by = 1)
  )) |> 
  mutate(frequency = if_else(is.na(frequency),
                             0,
                             frequency)) |> 
  mutate(scenario = "observation")

gp.cur = bind_rows(pred.partial, obs.partial) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("UM catch frequencies")+
  labs(subtitle = "from `preliminary-catch-model.R`, using negbin model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/total-UM-v2.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)


gp.cur = bind_rows(pred.partial, obs.partial) |> 
  filter(fish_count > 0) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("UM catch frequencies (skipping 0s to provide more clarity)")+
  labs(subtitle = "from `preliminary-catch-model.R`, using negbin model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/total-UM-v2-zoom.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)

## predicting many more 0s than we're seeing. Am I donig something wrong?

gp.fit = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(5,NA))+
  ggtitle("Observed counts of UM fish")+
  theme_bw(base_size = 14)

gp.pred = freq.pred |> 
  filter(fin_mark == "UM") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of UM fish")+
  xlim(c(5,NA))+
  theme_bw(base_size = 14)



ggsave(filename = here("figures/catch-model-results/total-UM-zoom-high.pdf"),
       plot = gp.fit / gp.pred & plot_annotation(title = "Negative binomial model",
                                                 subtitle = "from `preliminary-catch-model.R`"),
       height = 8, width = 8
)

gp.fit = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(5, max(dat.fit$fish_count +1)))+
  ggtitle("Observed counts of UM fish")+
  theme_bw(base_size = 14)

gp.pred = freq.pred |> 
  filter(fin_mark == "UM") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of UM fish")+
  xlim(c(5, max(dat.fit$fish_count +1)))+
  theme_bw(base_size = 14)



ggsave(filename = here("figures/catch-model-results/total-UM-zoom-high.pdf"),
       plot = gp.fit / gp.pred & plot_annotation(title = "Negative binomial model",
                                                 subtitle = "from `preliminary-catch-model.R`"),
       height = 8, width = 8
)

## quick thoughts:
freq.pred |> 
  filter(fin_mark == "UM") |> 
  filter(predicted_count > 10) |> 
  pull(expected_frequency) |> 
  sum()

## Checking totals =====================
sum(dat.fit$fish_count)
sum(freq.pred$predicted_count * freq.pred$expected_frequency)



## Looking at rounding bias ===============================


obs.partial = dat.fit |> 
  count(fin_mark, fish_count) |> 
  rename(frequency = n) |> 
  filter(fish_count %in% c(5, 10, 15, 20)) |> 
  ## adding 0s where missing
  full_join(expand_grid(fin_mark = c("AD", "UM"), 
                        fish_count = c(5, 10, 15, 20))
            ) |> 
  mutate(frequency = if_else(is.na(frequency),
                              0,
                             frequency)) |> 
  mutate(scenario = "observation")

pred.partial = freq.pred |> 
  filter(predicted_count %in% c(5, 10, 15, 20)) |> 
  mutate(scenario = "prediction") |> 
  select(fin_mark, fish_count = predicted_count, frequency = expected_frequency, scenario)

bind_rows(obs.partial, pred.partial) |> 
  ggplot(aes(x = as.factor(fish_count), y = frequency, fill = scenario))+
  geom_col(position = "dodge2")+
  labs(x = "Fish reported",
       title = "Looking for rounding bias: comparing frequencies")+
  facet_wrap(.~ fin_mark, ncol = 1)+
  theme_bw(base_size = 14)

## let's look at predictions vs obs by category

obs.partial = dat.fit |> 
  count(fin_mark, round_cat1) |> 
  rename(frequency = n) |> 
  mutate(scenario = "observation") |> 
  na.omit()

pred.partial = freq.pred |> 
  group_by(fin_mark, round_cat1) |> 
  summarize(frequency = sum(expected_frequency)) |> 
  ungroup() |> 
  mutate(scenario = "prediction") |> 
  na.omit()

bind_rows(obs.partial, pred.partial) |> 
  ggplot(aes(x = round_cat1, y = frequency, fill = scenario))+
  geom_col(position = "dodge2")+
  labs(x = "Rounding category",
       title = "Looking for rounding bias: comparing within categories")+
  facet_wrap(.~ fin_mark, ncol = 1)+
  theme_bw(base_size = 14)

## let's look at predictions vs obs by category

obs.partial = dat.fit |> 
  count(fin_mark, round_cat2) |> 
  rename(frequency = n) |> 
  mutate(scenario = "observation") |> 
  na.omit()

pred.partial = freq.pred |> 
  group_by(fin_mark, round_cat2) |> 
  summarize(frequency = sum(expected_frequency)) |> 
  ungroup() |> 
  mutate(scenario = "prediction") |> 
  na.omit()

bind_rows(obs.partial, pred.partial) |> 
  ggplot(aes(x = round_cat2, y = frequency, fill = scenario))+
  geom_col(position = "dodge2")+
  labs(x = "Rounding category",
       title = "Looking for rounding bias: comparing within categories (2)")+
  facet_wrap(.~ fin_mark, ncol = 1)+
  theme_bw(base_size = 14)



## Double checking that the issue is not in my implementation of my method:

res = numeric(31)
for(i in 1:nrow(dat.fit)){
  if(dat.fit$fin_mark[i] == "AD"){
    res = res + dnbinom(0:30, size = cur.theta, mu = mus[i])
  }
}

## Okay, so the model is predicting many more AD 0s than we're seeing. Does this make sense?
## It might! Anglers have to stop after catching quota, and they're motivated to catch at least one. So we might see 
## a bias away from having 0 UM. This would explain why our UM distribution seems a reasonable match for the predicted distribution wrt 0s,
## while our ADs are not. 
## 
## Do we see signs of rounding bias?
## What did we SEE?
dat.fit |> 
  filter(fin_mark == "UM") |> 
  filter(fish_count %in% c(5, 10, 15, 20)) |> 
  count(fish_count)
## what did the model predict we would see?
freq.pred |> 
  filter(fin_mark == "UM") |> 
  filter(predicted_count %in% c(5, 10, 15, 20)) 

# We DID see more reports with 10 UMs than we expected! We saw 9 and expected to see 0 (We thought there was a 10% chance of seeing 1)
# These could well be rounding errors. However, our model simply does not predict a high likelihood of 
# counts above 6 or 7: the model thinks there should only be a total of 1.12 occurences of reported UM counts above 7, so even if all anglers rounded all catch above 7 up (or down) to 10, we still have almost 9x more counts of 10 UM than we would expect:

freq.pred |> filter(fin_mark == "UM") |> filter(predicted_count>6) |> pull(expected_frequency) |> sum()

## let's take a quick look at those records
dat.fit |> 
  filter(fin_mark == "UM") |> 
  filter(fish_count == 10)
## of these 9 records, 5 of them reported at least 1 AD catch as well. This is less than the
## average frequency of records with at least 1 AD catch (75%):
mean(dat.fit$had.kept == "TRUE", na.rm = T)


## looking at prestige bias ------------------------
# 
# 
facet_names = c(`TRUE` = "Provably had catch",
            `FALSE` = "Did not provably have catch")



gp1 = df.pred |> 
  filter(fin_mark == "UM") |> 
  group_by(predicted_count, had.kept) |> 
  summarize(expected_frequency = sum(predicted_probability)) |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  facet_wrap(.~ had.kept, labeller = as_labeller(facet_names))+
  scale_y_log10()+
  xlim(c(-1, 16)) +
  ggtitle("Predicted count frequencies by had.kept")+
  theme_bw(base_size = 14)

gp2 = dat.fit |> 
  filter(fin_mark == "UM") |> 
  group_by(had.kept) |> 
  count(fish_count) |> 
  ungroup() |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  facet_wrap(.~ had.kept, labeller = as_labeller(facet_names)) +
  scale_y_log10()+
  xlim(c(-1, 16))+
  ggtitle("Observed count frequencies by had.kept")+
  theme_bw(base_size = 14)

gp1 / gp2

pred.partial = df.pred |> 
  filter(fin_mark == "UM") |> 
  group_by(predicted_count, had.kept) |> 
  summarize(expected_frequency = sum(predicted_probability)) |> 
  ungroup() |> 
  mutate(scenario = "prediction") |> 
  rename(fish_count = predicted_count, frequency = expected_frequency)

obs.partial = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(had.kept, fish_count) |>
  rename(frequency = n) |>
  ##fill in 0s
  full_join(expand_grid(had.kept = as.factor(c(TRUE, FALSE)), 
                        fish_count = seq(0, max(obs.partial$fish_count), by = 1)
  )) |> 
  mutate(frequency = if_else(is.na(frequency),
                             0,
                             frequency)) |> 
  mutate(scenario = "observation")

bind_rows(pred.partial, obs.partial) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  facet_wrap(.~ had.kept, labeller = as_labeller(facet_names),
             ncol = 1) +
  scale_y_log10()+
  xlim(c(-1, 20))+
  ggtitle("UM catch frequencies (log scale)")+
  labs(subtitle = "something funky -- missing 1 obs of 16 encounters")+
  theme_bw(base_size = 14)

gp.prestige = bind_rows(pred.partial, obs.partial) |> 
  filter(fish_count > 0) |>
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  facet_wrap(.~ had.kept, labeller = as_labeller(facet_names),
             ncol = 1, scale = "free_y") +
  xlim(c(-1, 20))+
  ggtitle("UM catch frequencies")+
  labs(subtitle = "excluding 0s for visual clarity")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/prliminary prestige bias fig.pdf"),
       plot = gp.prestige & plot_annotation(title = "Negative binomial model",
                                                 subtitle = "from `preliminary-catch-model.R`"),
       height = 8, width = 8
)


## Zero-inflated model ----------------------------------
# Looking at the predicted vs observed distributions of UM and AD, it looks like we're running into differing dispersion. 
# I think the best method is to try to account for this with a zero-inflated model


# Let's check out the nbinom2 family in glmmTMB
#
library(glmmTMB)

out.tmb1 = glmmTMB(fish_count ~ s(doy, fin_mark, bs = 'fs') + fishing_duration_minutes + (1 | yearfac),
                  ziformula = ~ fin_mark,
                  family = nbinom1,
                  REML = TRUE,
                  data = dat.fit)
out.tmb2 = glmmTMB(fish_count ~ s(doy, fin_mark, bs = 'fs') + fishing_duration_minutes + (1 | yearfac),
                   ziformula = ~ fin_mark,
                   family = nbinom2,
                   REML = TRUE,
                   data = dat.fit)

out.tmb3 = glmmTMB(fish_count ~ s(doy, fin_mark, bs = 'fs') + fishing_duration_minutes + (1 | yearfac),
                   ziformula = ~ fin_mark,
                   family = nbinom1,
                   dispformula = ~fin_mark,
                   REML = TRUE,
                   data = dat.fit)
out.tmb4 = glmmTMB(fish_count ~ s(doy, fin_mark, bs = 'fs') + fishing_duration_minutes + (1 | yearfac),
                   ziformula = ~ fin_mark,
                   family = truncated_nbinom1,
                   dispformula = ~fin_mark,
                   REML = TRUE,
                   data = dat.fit)
out.tmb4b = glmmTMB(fish_count ~ s(doy, fin_mark, bs = 'fs') + fishing_duration_minutes + (1 | yearfac),
                   ziformula = ~ fin_mark,
                   family = truncated_nbinom1,
                   REML = TRUE,
                   data = dat.fit)

AIC(out.tmb1, out.tmb2, out.tmb3, out.tmb4, out.tmb4b)

## tmb1 is the way to go. Plus, it's more consistent with our work above.

mus = predict(out.tmb3, newdata = dat.fit, type = "conditional")
probs.struct.zero = predict(out.tmb3, newdata = dat.fit, type = "zprob") 
dispersion.vals = predict(out.tmb3, newdata = dat.fit, type = "disp") 
#For nbinom1: V=\mu(1+\phi)
#For nbinom2:  V=\mu(1+\mu/\phi) = \mu+\mu^2/\phi

cli::cli_alert("It appears that the dispersional value as pulled out of the model with sigma() is equivalent to the size parameter of the dnbinom function. This is worth checking")

foo <- function(mu, disp.val, zprob){
  non.struct = dnbinom(0:30, size = disp.val, mu = mu)
  struct = c(1, rep(0,30))
  return(non.struct * (1 - zprob) + struct * zprob)
  }
## this gets messy



df = data.frame(mu = mus,
                disp.val = dispersion.vals,
                probs.struct.zero
                )|> 
  mutate(mat = do.call(rbind, purrr::pmap(
    list(mu = mu, disp.val = disp.val, zprob = probs.struct.zero),
    foo)))

df.pred = as.data.frame(df$mat)
#gros names for a sec:
names(df.pred) = 0:30  
## now combine with the data and use pivoting to put into handy form
df.pred <- dat.fit |> 
  select(-fish_count_predicted, -fish_count_predicted_int) |> 
  cbind(df.pred) |> 
  pivot_longer(cols = "0":"30",
               names_to = "predicted_count",
               values_to = "predicted_probability") |> 
  mutate(predicted_count = as.numeric(predicted_count))

## With this dataframe, we can now sum up the number of counts we would expect to see
## for marked and unmarked fish, *accounting* for the variability in negative binomials

freq.pred = df.pred |> 
  group_by(fin_mark, predicted_count) |> 
  summarize(expected_frequency = sum(predicted_probability)) |> 
  ungroup() |> 
  ## adding rounding categorizations
  mutate(round_cat1 = case_when(
    predicted_count >= 3 & predicted_count <= 7 ~ "3-7",
    predicted_count >= 8 & predicted_count <= 12 ~ "8-12",
    predicted_count >= 13 ~ "13+",
  )) |> 
  mutate(round_cat1 = factor(round_cat1, levels = c("3-7", "8-12", "13+"))) |> 
  mutate(round_cat2 = case_when(
    predicted_count >= 3 & predicted_count <= 7 ~ "3-7",
    predicted_count >= 8 ~ "8+"
  )) |> 
  mutate(round_cat2 = factor(round_cat2, levels = c("3-7", "8+")))

## basic plotting ================================

pred.partial = freq.pred |> 
  filter(fin_mark == "AD") |> 
  group_by(predicted_count) |> 
  summarize(expected_frequency = sum(expected_frequency)) |> 
  ungroup() |> 
  mutate(scenario = "prediction") |> 
  rename(fish_count = predicted_count, frequency = expected_frequency)

obs.partial = dat.fit |> 
  filter(fin_mark == "AD") |> 
  count(fish_count) |>
  rename(frequency = n) |>
  ##fill in 0s
  full_join(expand_grid(fish_count = seq(0, max(dat.fit$fish_count), by = 1)
  )) |> 
  mutate(frequency = if_else(is.na(frequency),
                             0,
                             frequency)) |> 
  mutate(scenario = "observation")

gp.cur = bind_rows(pred.partial, obs.partial) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("AD catch frequencies")+
  labs(subtitle = "from `preliminary-catch-model.R`, using zero-inflated model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/zinf-total-AD-v2-zinf.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)


gp.cur = bind_rows(pred.partial, obs.partial) |> 
  filter(fish_count > 0) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("AD catch frequencies (skipping 0s to provide more clarity)")+
  labs(subtitle = "from `preliminary-catch-model.R`, using zero-inflated model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/zinf-total-AD-v2-zoom.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)




pred.partial = freq.pred |> 
  filter(fin_mark == "UM") |> 
  group_by(predicted_count) |> 
  summarize(expected_frequency = sum(expected_frequency)) |> 
  ungroup() |> 
  mutate(scenario = "prediction") |> 
  rename(fish_count = predicted_count, frequency = expected_frequency)

obs.partial = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |>
  rename(frequency = n) |>
  ##fill in 0s
  full_join(expand_grid(fish_count = seq(0, max(dat.fit$fish_count), by = 1)
  )) |> 
  mutate(frequency = if_else(is.na(frequency),
                             0,
                             frequency)) |> 
  mutate(scenario = "observation")

gp.cur = bind_rows(pred.partial, obs.partial) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("UM catch frequencies")+
  labs(subtitle = "from `preliminary-catch-model.R`, using zero-inflated model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/zinf-total-UM-v2.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)


gp.cur = bind_rows(pred.partial, obs.partial) |> 
  filter(fish_count > 0) |> 
  ggplot(aes(x = fish_count, y = frequency, fill = scenario))+
  geom_col(position = "dodge2", width = .6)+
  ggtitle("UM catch frequencies (skipping 0s to provide more clarity)")+
  labs(subtitle = "from `preliminary-catch-model.R`, using negbin model")+
  theme_bw(base_size = 14)

ggsave(filename = here("figures/catch-model-results/total-UM-v2-zoom.pdf"),
       plot = gp.cur,
       height = 8, width = 12
)

## Quick check
## 


gp.fit = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(5,NA))+
  ggtitle("Observed counts of UM fish")+
  theme_bw(base_size = 14)

gp.pred = freq.pred |> 
  filter(fin_mark == "UM") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of UM fish")+
  xlim(c(5,NA))+
  theme_bw(base_size = 14)



ggsave(filename = here("figures/catch-model-results/total-UM-zoom-high.pdf"),
       plot = gp.fit / gp.pred & plot_annotation(title = "Negative binomial model",
                                                 subtitle = "from `preliminary-catch-model.R`"),
       height = 8, width = 8
)

gp.fit = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(5, max(dat.fit$fish_count +1)))+
  ggtitle("Observed counts of UM fish")+
  theme_bw(base_size = 14)

gp.pred = freq.pred |> 
  filter(fin_mark == "UM") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of UM fish")+
  xlim(c(5, max(dat.fit$fish_count +1)))+
  theme_bw(base_size = 14)



ggsave(filename = here("figures/catch-model-results/total-UM-zoom-high.pdf"),
       plot = gp.fit / gp.pred & plot_annotation(title = "Negative binomial model",
                                                 subtitle = "from `preliminary-catch-model.R`"),
       height = 8, width = 8
)

## quick thoughts:
freq.pred |> 
  filter(fin_mark == "UM") |> 
  filter(predicted_count > 10) |> 
  pull(expected_frequency) |> 
  sum()

## Checking totals =====================
sum(dat.fit$fish_count)
sum(freq.pred$predicted_count * freq.pred$expected_frequency)








## Example code for methods --------------------------------------------------------

## Current next step:
##    Try to predict the PDF for each individual observation, rather than the point estimate. Want to sum those up in some way
##    so that I can create a more reasonable histogram of "expected" vs "observed". Point estimates of "expected" are going to represent
##    an average-ish count in each day. 
##    
##    As I recall, current challenge is obtaining the theta estimates
##    I figured this out! out$family$getTheta() If "FALSE", returns on link scale. If "TRUE", returns on response scale


temp  <-  predict(out, newdata = dat.fit, type = "response")

cur.theta <- out$family$getTheta(TRUE)

## Okay, how do I find the probabilities of different counts? dnbinom

dnbinom(x = 0:10, size = cur.theta, mu = temp[1])

## am I doing this right? Should I be on the link scale for this calculation?

## We can test!

dat = data.frame(obs = rnbinom(10000, size = 3, mu = 2))

out.test= gam(obs ~ 1, method = "REML",
              family = "nb",
              data = dat)

temp  <-  predict(out.test, newdata = dat, type = "response")
cur.theta <- out.test$family$getTheta(TRUE)

data.frame(count = 0:10, 
           predicted = round(dnbinom(0:10, size = cur.theta, mu = temp[1]), 4),
           observed = round(as.numeric(table(dat$obs)/nrow(dat))[1:11], 4)
)




# Question for evan: are interviews with 0 0 included?