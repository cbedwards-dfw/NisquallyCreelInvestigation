##################################################
## Collin Edwards
## Thu Jul 17 11:59:41 2025
## Work with models fitted in `catch-model-fitting.R`
##################################################
library(here)
library(tidyverse)

fitted_models = readRDS(file = here("fitted_models/fitted_models_all.RDS"))
model_details = read_csv(file = here("fitted_models/fitted_models_info_all.csv"))

summary(fitted_models$model[[14]])
