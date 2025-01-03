##################################################
## Collin Edwards
## Thu Jan  2 13:49:40 2025
## Preliminary phenology model of net data
##################################################
library(tidyverse)
library(here)
library(ggridges)
library(mgcv)
library(DHARMa)

dat = read_csv(here("cleaned_data/tidy_20-years-net-mark-rates.csv"))

dat |> 
  group_by(year, week) |> 
  summarize(catch = sum(catch)) |> 
  ungroup() |> 
  mutate(year = as.factor(year)) |> 
  ggplot(aes(x = week, y = year, height = catch, group = year))+
  scale_y_discrete(limits = rev)+
  geom_density_ridges(scale = 1, stat = "identity")+
  theme_bw(base_size = 18)+
  ggtitle("Treaty net total catch by week")

## Basic modeling

dat.model = dat |> 
  group_by(year, week) |> 
  summarize(catch = sum(catch)) |> 
  ungroup() |> 
  mutate(year = as.factor(year))

out = gam(catch ~ year + s(week, by = year, k = 5),
          method = "REML",
          family = nb,
          data = dat.model)

dat.pred = expand_grid(week = unique(dat.model$week), year = unique(dat.model$year))
dat.pred$catch = predict(out, newdata = dat.pred, type = "response")

ggplot(data = dat.pred, aes(x = week, y = catch))+
  geom_path()+
  geom_point(data = dat.model, col = 'blue')+
  facet_wrap(. ~ year, scale = "free_y")

dat.meds = dat.model |> 
  group_by(year) |> 
  summarize(med.week = median(rep(week, times = catch))) |> 
  ungroup()

ggplot(dat.meds, aes(x = as.numeric(as.character(year)), y = med.week))+
  geom_point(size = 3)+
  ggtitle("Median week of treaty net Chinook catch")+
  xlab("year")+
  ylab("Week of median catch")+
  theme_bw(base_size = 16)
