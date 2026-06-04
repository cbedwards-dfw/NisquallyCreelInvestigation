library(here)
library(mgcv)
library(tidyverse)
library(gamm4)

## Model-running
## GAM model -------------------------------------------

dat.fit = read_csv(here("cleaned_data/dat-fit.csv")) |> 
  mutate(across(c(fin_mark, life_stage, mark_stage, interview_id_fac, boat_used, yearfac), ~ as.factor(.x)))



model_verbosity = function(){
  bullet.text = c("*" = paste0("Fish handling: ", fish.type),
                  "*" = paste0("Effort handling: ", effort.type),
                  "*" = paste0("Smooth approach:", smooth.type),
                  "*" = paste0("Interview-specific random effect = ", interview.re),
                  "*" = paste0("Fitting with ", method))
  cli::cli_div(theme = list(span.emph = list(color = "blue")))
  cli::cli_alert("{.emph Fitting model with following options:}")
  cli::cli_bullets(bullet.text)
  print(Sys.time())
}


fish.type = "fin_mark * life_stage"
effort.type = "boat_used + angler_minutes"
smooth.type = "across doy by mark_stage"
interview.re = FALSE
method = "gam"
model_verbosity()
start.time = Sys.time()
out = gam(fish_count ~ fin_mark*life_stage +
            s(doy, by = mark_stage, k = 20) + 
            yearfac + 
            boat_used + angler_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)
models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                    fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))


fish.type = "fin_mark * life_stage"
effort.type = "boat_used * angler_minutes"
smooth.type = "across doy by mark_stage"
interview.re = FALSE
method = "gam"
model_verbosity()
start.time = Sys.time()
out = gam(fish_count ~ fin_mark*life_stage +
            s(doy, by = mark_stage, k = 20) + 
            yearfac + 
            boat_used * angler_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )


fish.type = "fin_mark * life_stage"
effort.type = "boat_used + angler_minutes"
smooth.type = "across doy by fin_mark"
interview.re = FALSE
method = "gam"
model_verbosity()
start.time = Sys.time()
out = gam(fish_count ~ fin_mark*life_stage +
              s(doy, by = fin_mark, k = 20) + 
              yearfac + 
              boat_used + angler_minutes,
            method = "REML",
            family = "nb",
            data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )


fish.type = "fin_mark"
effort.type = "boat_used + angler_minutes"
smooth.type = "across doy by fin_mark"
interview.re = FALSE
method = "gam"
model_verbosity()
start.time = Sys.time()
out = gam(fish_count ~ fin_mark +
              s(doy, by = fin_mark, k = 20) + 
              yearfac + 
              boat_used + angler_minutes,
            method = "REML",
            family = "nb",
            data = dat.fit)
models_df = models_df |> 
  bind_rows(
        models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )


fish.type = "within smooth"
effort.type = "boat_used + angler_minutes"
smooth.type = "te of doy, mark_stage, yearfac"
interview.re = FALSE
method = "gam"
model_verbosity()
start.time = Sys.time()
out = gam(fish_count ~ 
            te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
            boat_used + angler_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )

fish.type = "within smooth"
effort.type = "boat_used:angler_minutes"
smooth.type = "te of doy, mark_stage, yearfac"
interview.re = FALSE
method = "gam"
model_verbosity()
start.time = Sys.time()
out = gam(fish_count ~ 
            te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
            boat_used:angler_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )

fish.type = "within smooth"
effort.type = "boat_used*angler_minutes"
smooth.type = "te of doy, mark_stage, yearfac"
interview.re = FALSE
method = "gam"
model_verbosity()
start.time = Sys.time()
out = gam(fish_count ~ 
            te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
            boat_used*angler_minutes,
          method = "REML",
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )


## view results
models_details = models_df |> select(-model)
# View(models_details)

saveRDS(models_df, file = here("fitted_models/fitted_models_gamonly.RDS"))
write_csv(models_details, file = here("fitted_models/fitted_models_info.csv"))


# temp = readRDS(file = here("fitted_models/fitted_models.RDS"))

### BAM models------------------------------
fish.type = "fin_mark * life_stage"
effort.type = "boat_used + angler_minutes"
smooth.type = "across doy by mark_stage"
interview.re = TRUE
method = "bam, fREML, discrete"

# model_verbosity()
# start.time = Sys.time()
# out = gamm(fish_count ~ fin_mark*life_stage +
#             s(doy, by = mark_stage, k = 20) +
#             s(interview_id_fac, bs ='re')+
#             yearfac + 
#             boat_used + angler_minutes,
#           method = "REML",
#           family = "nb",
#           data = dat.fit)
#  (Sys.time()-start.time)/60

model_verbosity()
start.time = Sys.time()
out2 = gamm4::gamm4(fish_count ~ fin_mark*life_stage +
             s(doy, by = mark_stage, k = 20) +
             yearfac + 
             boat_used + angler_minutes,
             random = ~(1|interview_id_fac),
           family = "poisson",
           data = dat.fit)
(Sys.time()-start.time)/60
  
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )
saveRDS(models_df, file = here("fitted_models/fitted_models_ongoing.RDS"))

fish.type = "fin_mark * life_stage"
effort.type = "boat_used * angler_minutes"
smooth.type = "across doy by mark_stage"
interview.re = TRUE
method = "bam, fREML, discrete"
model_verbosity()
start.time = Sys.time()
out = bam(fish_count ~ fin_mark*life_stage +
            s(doy, by = mark_stage, k = 20) + 
            s(interview_id_fac, bs ='re')+
            yearfac + 
            boat_used * angler_minutes,
          method = "fREML",
          discrete = TRUE,
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )
saveRDS(models_df, file = here("fitted_models/fitted_models_ongoing.RDS"))


fish.type = "fin_mark * life_stage"
effort.type = "boat_used + angler_minutes"
smooth.type = "across doy by fin_mark"
interview.re = TRUE
method = "bam, fREML, discrete"
model_verbosity()
start.time = Sys.time()
out = bam(fish_count ~ fin_mark*life_stage +
            s(doy, by = fin_mark, k = 20) + 
            s(interview_id_fac, bs ='re')+
            yearfac + 
            boat_used + angler_minutes,
          method = "fREML",
          discrete = TRUE,
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )
saveRDS(models_df, file = here("fitted_models/fitted_models_ongoing.RDS"))


fish.type = "fin_mark"
effort.type = "boat_used + angler_minutes"
smooth.type = "across doy by fin_mark"
interview.re = TRUE
method = "bam, fREML, discrete"
model_verbosity()
start.time = Sys.time()
out = bam(fish_count ~ fin_mark +
            s(doy, by = fin_mark, k = 20) + 
            s(interview_id_fac, bs ='re')+
            yearfac + 
            boat_used + angler_minutes,
          method = "fREML",
          discrete = TRUE,
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )
saveRDS(models_df, file = here("fitted_models/fitted_models_ongoing.RDS"))


fish.type = "within smooth"
effort.type = "boat_used + angler_minutes"
smooth.type = "te of doy, mark_stage, yearfac"
interview.re = TRUE
method = "bam, fREML, discrete"
model_verbosity()
start.time = Sys.time()
out = bam(fish_count ~ 
            te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
            s(interview_id_fac, bs ='re')+
            boat_used + angler_minutes,
          method = "fREML",
          discrete = TRUE,
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )
saveRDS(models_df, file = here("fitted_models/fitted_models_ongoing.RDS"))

fish.type = "within smooth"
effort.type = "boat_used:angler_minutes"
smooth.type = "te of doy, mark_stage, yearfac"
interview.re = TRUE
method = "bam, fREML, discrete"
model_verbosity()
start.time = Sys.time()
out = bam(fish_count ~ 
            te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
            s(interview_id_fac, bs ='re')+
            boat_used:angler_minutes,
          method = "fREML",
          discrete = TRUE,
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )
saveRDS(models_df, file = here("fitted_models/fitted_models_ongoing.RDS"))

fish.type = "within smooth"
effort.type = "boat_used*angler_minutes"
smooth.type = "te of doy, mark_stage, yearfac"
interview.re = TRUE
method = "bam, fREML, discrete"
model_verbosity()
start.time = Sys.time()
out = bam(fish_count ~ 
            te(doy, mark_stage, yearfac, bs = c('tp', 'fs', 'fs')) + 
            s(interview_id_fac, bs ='re')+
            boat_used*angler_minutes,
          method = "fREML",
          discrete = TRUE,
          family = "nb",
          data = dat.fit)
models_df = models_df |> 
  bind_rows(
    models_df = tribble(~fish.type, ~effort.type, ~smooth.type, ~interview.re, ~error, ~method, ~model, ~runtime, ~AIC,
                        fish.type, effort.type, smooth.type, interview.re, "nb", method, out, Sys.time() - start.time, AIC(out))
  )


## view results
models_details = models_df |> select(-model)
# View(models_details)

saveRDS(models_df, file = here("fitted_models/fitted_models_all.RDS"))
write_csv(models_details, file = here("fitted_models/fitted_models_info_all.csv"))








