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

## rerun the following to recreate the clean data
if(FALSE){
  source(here("scripts/prepare_creel_data.R"))
}

raw = read_csv(here("cleaned_data/key_dataframes/creel_interview_catch_withzeros.csv")) 
## Note: problem with col 4 being interpretted as a T/F instead of char

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
  group_by(interview_id, fin_mark, event_date, year, month, fishing_duration_minutes, fate) |>   
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
  filter(!is.na(fishing_duration_minutes))

cli::cli_alert("Reminder: skipping observations with no reported fishing time. These are ~ 500 out of 63,500, so not super worried about it, but in theory we could replace those with the average time. Consult with Evan.")

## want to also have time on the water

## consider adding random effect of interview!

out = gam(fish_count ~ fin_mark + s(doy, by = fin_mark) + s(yearfac, bs = 're') + fishing_duration_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)

draw(out)

library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = out, plot = F)
plot(simulationOutput)


dat.fit$fish_count_predicted = as.numeric(predict(out, newdata = dat.fit, type = "response"))
dat.fit = dat.fit |> 
  mutate(fish_count_predicted_int = round(fish_count_predicted))

dat.res = dat.fit |> 
  pivot_wider(names_from = fin_mark,
              values_from = c(fish_count, fish_count_predicted, fish_count_predicted_int))

ggplot(dat.res, aes(x = fish_count_UM, y = fish_count_predicted_int_UM, col = had.ad))+
  geom_jitter()

dat.res |> 
  ggplot(aes(x = fish_count_UM))+
  geom_histogram()+
  facet_grid(. ~ had.kept)

dat.res |> 
  ggplot(aes(x = fish_count_predicted_int_UM))+
  geom_histogram()+
  facet_grid(. ~ had.kept)

dat.res |> 
  count(fish_count_predicted_int_AD)
dat.res |> 
  count(fish_count_AD)

hist(dat.res$fish_count_predicted_AD)
hist(dat.res$fish_count_AD)

# ## sure looks like we have zero inflated data. Let's check out the nbinom2 family in glmmTMB
# 
# library(glmmTMB)
# 
# out.tmb = glmmTMB(fish_count ~ s(doy, fin_mark, bs = 'fs') + fishing_duration_minutes + (1 | yearfac) + (1 | interview_id),
#                   ziformula = ~ 1,
#                   family = "poisson",
#                   REML = TRUE,
#                   data = dat.fit)
# 
# simulationOutput <- simulateResiduals(fittedModel = out.tmb, plot = F)
# plot(simulationOutput)
# 
# dat.fit$fish_count_predicted = as.numeric(predict(out.tmb, newdata = dat.fit, type = "response"))
# dat.fit = dat.fit |> 
#   mutate(fish_count_predicted_int = round(fish_count_predicted))
# 
# dat.res = dat.fit |> 
#   pivot_wider(names_from = fin_mark,
#               values_from = c(fish_count, fish_count_predicted, fish_count_predicted_int))
# 
# ggplot(dat.res, aes(x = fish_count_UM, y = fish_count_predicted_int_UM, col = had.ad))+
#   geom_jitter()
# 
# dat.res |> 
#   ggplot(aes(x = fish_count_UM))+
#   geom_histogram()+
#   facet_grid(. ~ had.ad)
# 
# dat.res |> 
#   ggplot(aes(x = fish_count_predicted_int_UM))+
#   geom_histogram()+
#   facet_grid(. ~ had.ad)
# hist(dat.res$fish_count_predicted_AD)
# hist(dat.res$fish_count_AD)
# 
# dat.fit$residuals = residuals(out.tmb)
# 
# ggplot(dat.fit, aes(x = fish_count, y = residuals, col = had.ad))+
#   geom_jitter()+
#   facet_wrap(.~fin_mark)
# 
# dat.fit |> 
#   filter(fin_mark == "UM") |> 
#   ggplot(aes(x = residuals))+
#   geom_histogram() +
#   facet_wrap(.~had.ad)
# 
# temp1 = dat.fit |>
#   count(had.ad, fish_count, fin_mark) |> 
#   mutate(scenario = "observed")
# temp2 = dat.fit |> 
#   count(had.ad, fish_count_predicted_int, fin_mark) |> 
#   mutate(scenario = "predicted") |> 
#   rename(fish_count = fish_count_predicted_int)
# 
# rbind(temp1, temp2) |> 
#   ggplot(aes(x = fish_count, y = n, fill = scenario))+
#   geom_col(position = "dodge")+
#   facet_wrap(. ~ fin_mark)+
#   theme_bw()
# 
# ## model fit doesn't seem great either way. I could argue that UM is "more" off, but iit's not like AD is great eitehr. 
# 
# 
# ggplot(dat.fit |> 
#          filter(fish_count == 0),
#        aes(x = event_date))+
#   geom_density()+
#   facet_wrap(.~year, scale = "free_x")
# 
# 
# 
# temp = predict(out.tmb, newdata = dat.fit, type = "zprob")


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
  ungroup()

gp.fit = dat.fit |> 
  filter(fin_mark == "AD") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(-1,11))+
  ggtitle("Observed counts of AD fish")

gp.pred = freq.pred |> 
  filter(fin_mark == "AD") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of AD fish")+
  xlim(c(-1,11))

gp.fit / gp.pred

## predicting many more 0s than we're seeing. Am I donig something wrong?

gp.fit = dat.fit |> 
  filter(fin_mark == "UM") |> 
  count(fish_count) |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  xlim(c(-1,11))+
  ggtitle("Observed counts of UM fish")

gp.pred = freq.pred |> 
  filter(fin_mark == "UM") |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  ggtitle("Predicted counts of UM fish")+
  xlim(c(-1,11))

gp.fit / gp.pred

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


## looking for misreporting ---------------
## While we're here: For UM counts, how does the observed compare to the predicted when we split it by whether or not they had an AD fish?
# freq.pred

gp1 = df.pred |> 
  filter(fin_mark == "UM") |> 
  group_by(predicted_count, had.kept) |> 
  summarize(expected_frequency = sum(predicted_probability)) |> 
  ggplot(aes(x = predicted_count, y = expected_frequency))+
  geom_col()+
  facet_wrap(.~ had.kept)+
  scale_y_log10()+
  xlim(c(-1, 16)) +
  ggtitle("Predicted count frequencies by had.kept")

gp2 = dat.fit |> 
  filter(fin_mark == "UM") |> 
  group_by(had.kept) |> 
  count(fish_count) |> 
  ungroup() |> 
  ggplot(aes(x = fish_count, y = n))+
  geom_col()+
  facet_wrap(.~ had.kept) +
  scale_y_log10()+
  xlim(c(-1, 16))+
  ggtitle("Observed count frequencies by had.kept")

gp1 / gp2


# Question for evan: are interviews with 0 0 included?