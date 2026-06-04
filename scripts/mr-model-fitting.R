library(here)
library(mgcv)
library(gratia)
library(tidyverse)
library(ggh4x)

doy_2md=function(i){
  ymd=as.Date(i-1, origin="2019-01-01")
  return(format(ymd, "%b %d"))
}
## Model-running
## GAM model -------------------------------------------

dat.fit = read_csv(here("cleaned_data/dat-fit.csv")) |> 
  mutate(across(c(fin_mark, life_stage, mark_stage, interview_id_fac, boat_used, yearfac), ~ as.factor(.x))) |> 
  mutate(fishing_time_median_hr = (fishing_end_time-fishing_start_time)/2 + fishing_start_time) |> 
  mutate(fishing_time_median_hr = as.numeric(fishing_time_median_hr/60/60))


dat.sum = read_csv(here("cleaned_data/dat-sum.csv")) |> 
  mutate(across(c(interview_id, boat_used), ~ as.factor(.x))) |> 
  mutate(yearfac = as.factor(year)) |> 
  mutate(fishing_time_median_hr = (fishing_end_time-fishing_start_time)/2 + fishing_start_time) |> 
  mutate(fishing_time_median_hr = as.numeric(fishing_time_median_hr/60/60)) |> 
  mutate(doy = yday(event_date))


out = gam(fish_count ~ 
            te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
            boat_used*angler_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)

## trying fishing_time as a smooth showed a clean linear relationship for fishing time on bank anglers, and a wiggly mostly meaningless relationship for boat anglers. Want to avoid overfitting
out2 = gam(fish_count ~ 
             te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
             fishing_time_median_hr:boat_used +
             boat_used*angler_minutes,
           method = "REML",
           family = "nb",
           data = dat.fit)

AIC(out, out2)

draw(out2, rug = TRUE, scales = "free")+
  xlim(c(215, 300))

temp +xlim()

anova(out2)

dat.fit |> 
  select(year, doy, interview_id) |> 
  distinct() |> 
  group_by(year, doy) |> 
  summarize(n = n()) |> 
  mutate(n_cumulative = cumsum(n)) |> 
  ungroup() |> 
  ggplot(aes(x = doy, y = n_cumulative))+
  geom_path()+
  facet_wrap(.~ year)

dat.sum.use = dat.sum |> 
  select(ad_total, um_total, yearfac, boat_used, fishing_time_median_hr, doy) |> 
  na.omit()

out = gam(cbind(ad_total, um_total) ~ 
            yearfac + 
            s(doy, by = yearfac, k = 20)+
             boat_used + fishing_time_median_hr:boat_used,
           method = "REML",
           family = "binomial",
           data = dat.sum.use)
summary(out)
anova(out)

out1 = gam(cbind(ad_total, um_total) ~ 
             yearfac + 
             s(doy, by = yearfac, k = 20),
           method = "REML",
           family = "binomial",
           data = dat.sum.use)

AIC(out, out1)
anova(out, out1)

dat.pred = expand_grid(doy = seq(215, 300, by = 1),
                       yearfac = unique(dat.sum$year),
                       boat_used = unique(dat.sum$boat_used),
                       fishing_time_median_hr = median(dat.sum$fishing_time_median_hr, na.rm = T))
dat.pred = dat.pred |>  select(yearfac, doy) |> distinct()
dat.pred$mark_rate = predict(out1, 
                             newdata = dat.pred, 
                             type = "response")

ggplot(dat.pred, aes(x = doy, y = mark_rate))+
  geom_path()+
  # facet_grid(vars(boat_used), vars(yearfac))+
  facet_wrap(.~yearfac)+
  scale_x_continuous(labels = doy_2md)+
  labs(title = "Mark rates across years")+
  theme_bw(base_size = 13)
