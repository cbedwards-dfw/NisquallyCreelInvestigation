# catch tables
library(tidyverse)
library(DBI)
library(gt)
library(creelutils)

fishery <- "Nisqually salmon 2023"

creel_estimates <- list()

# establish connection
con <- DBI::dbConnect(odbc::odbc(), dsn = "creel_estimates")
# con <- creelutils::establish_db_con()

# query analysis_lut ####
creel_estimates$analysis_lut <- creelutils::fetch_db_table(
  con, "creel", "model_analysis_lut") |> 
  # fishery name ends at the first underscore of analysis name field
  mutate(fishery_name = stringr::str_extract(analysis_name, "^[^_]+")) |> 
  relocate(fishery_name, .before = "analysis_name") |> 
  filter(fishery_name == fishery) |> 
  mutate(upload_date = lubridate::as_date(created_datetime), .after = "created_datetime")

analysis_ids <- creel_estimates$analysis_lut |> arrange(upload_date) |> select(analysis_id) |> pull()

# query estimates ####
creel_estimates$total <- creelutils::fetch_db_table(con, "creel", "vw_model_estimates_total") |> 
  filter(analysis_id %in% analysis_ids)

creel_estimates$stratum <- creelutils::fetch_db_table(con, "creel", "vw_model_estimates_stratum") |> 
  filter(analysis_id %in% analysis_ids)

#create catch group to analysis id crosswalk table
catch_groups <- tibble(
  analysis_id = c(
    analysis_ids[1], #896, BSS estimates where one catch group was run at a time
    analysis_ids[2], #67C
    analysis_ids[3], #ABA
    analysis_ids[4], #240
    rep(analysis_ids[5], 6) #335D, PE estimates where multiple catch groups were run together
  ),
  catch_group = c(
    "Chinook_Adult_AD_Kept",
    "Chinook_Adult_AD_Released",
    "Chinook_Adult_UM_Released",
    "Chinook_Jack_AD_Kept",
    "Chinook_Adult_UM_Kept",
    "Chinook_Adult_UNK_Released",
    "Chinook_Jack_AD_Released",
    "Chinook_Jack_UM_Released",
    "Chinook_Jack_UNK_Kept",
    "Chinook_Jack_UNK_Released"
  ),
  model_type = c(
    rep("BSS", 4), #4 catch groups with BSS
    rep("PE", 6) #6 catch groups with PE
  )
)

bss_ids <- catch_groups |> filter(model_type == "BSS")
pe_ids <- catch_groups |> filter(model_type == "PE")

# download raw data ####
raw_data <- creelutils::fetch_dwg("Nisqually salmon 2023")


# format estimates a little
report_estimates <- list()

# add useful date variables
report_estimates$total <- creel_estimates$total |> 
  mutate(
    month = lubridate::month(min_event_date),
    year = lubridate::year(min_event_date),
    week = lubridate::week(min_event_date),
    julian_date = lubridate::yday(.data$min_event_date)
  ) |> 
  rename(species = species_name, life_stage = life_stage_name, fin_mark = fin_mark_desc, fate = fate_name) |> 
  mutate(
    # fin_mark = case_when(
    #   fin_mark == "AD" ~ "Adipose fin clipped",
    #   fin_mark == "UM" ~ "Unmarked",
    #   fin_mark == "UNK" ~ "Unknown",
    #   TRUE ~ fin_mark
    # ),
    catch_group = paste0(species, "_", life_stage, "_", fin_mark, "_", fate)
  ) 

report_estimates$stratum <- creel_estimates$stratum |> 
  mutate(
    month = lubridate::month(min_event_date),
    year = lubridate::year(min_event_date),
    week = lubridate::week(min_event_date),
    julian_date = lubridate::yday(.data$min_event_date)
  ) |> 
  rename(species = species_name, life_stage = life_stage_name, fin_mark = fin_mark_desc, fate = fate_name) |> 
  mutate(
    # fin_mark = case_when(
    #   fin_mark == "AD" ~ "Adipose fin clipped",
    #   fin_mark == "UM" ~ "Unmarked",
    #   fin_mark == "UNK" ~ "Unknown",
    #   TRUE ~ fin_mark
    # ),
    catch_group = paste0(species, "_", life_stage, "_", fin_mark, "_", fate)
  ) 

# total effort table ####
total_effort <- report_estimates$total |> 
  filter(
    model_type %in% c("BSS","PE"),
    estimate_category %in% c("effort","E_sum"),
    estimate_type %in% c(
      "estimate_sum", "mean", "standard_deviation", 
      "quantile_median_50", "quantile_lower_2_5", "quantile_upper_97_5"
    )) |> 
  group_by(year, model_type, estimate_type) |> 
  summarise(estimate_value = mean(estimate_value, .groups = "drop"))  # average over multiple BSS runs

#create table
total_effort_table <-
  total_effort |> 
  ungroup() |> 
  select(-model_type) |> 
  pivot_wider(names_from = estimate_type, values_from = estimate_value) |> 
  mutate(CV = (standard_deviation / mean)) |> 
  rename(
    Year = "year",
    `Design-based estimator` = "estimate_sum", 
    `Mean` = "mean",
    `SD` = "standard_deviation",
    `50%` = "quantile_median_50",
    `2.5%` = "quantile_lower_2_5",
    `97.5%` = "quantile_upper_97_5"
  )

total_effort_table  |> 
  gt() |> 
  fmt_number(columns = c("Design-based estimator", "Mean", "SD", `2.5%`, `50%`, `97.5%`), decimals = 0) |> 
  fmt_percent("CV", decimals = 1) |> 
  cols_align(columns = "Design-based estimator", align = "center") |>
  cols_width("Design-based estimator" ~ px(120)) |> 
  tab_spanner(
    label = "Bayesian state-space model",
    columns = c("Mean", "SD", "CV", `2.5%`, `50%`, `97.5%`)
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
  )

# daily effort plot ####

# add approximate julian date - month matches 
month_ticks <- data.frame(
  julian_date = c(182, 213, 244, 274, 305, 319),  # July 1, Aug 1, Sep 1, Oct 1, Nov 1, Nov 15
  month_label = c("Jul", "Aug", "Sep", "Oct", "Nov", "")
)

# wrangle the data for plotting 
effort_daily <- report_estimates$stratum |> 
  mutate(angler_type_name = word(angler_type_name)) |> 
  mutate(angler_type_name = factor(angler_type_name, levels = c("Bank", "Boat"))) |> 
  # mutate(catch_group = paste(species_name, life_stage_name, fate_name, fin_mark_desc, sep = "_")) |> 
  filter(estimate_category == "effort") |> 
  filter(estimate_type %in% c("quantile_lower_2_5", "quantile_median_50", "quantile_upper_97_5")) |> 
  pivot_wider(names_from = estimate_type, values_from = estimate_value) |> 
  group_by(year, month, julian_date, angler_type_name) |> 
  summarise(
    quantile_lower_2_5 = mean(quantile_lower_2_5),
    quantile_median_50 = mean(quantile_median_50),
    quantile_upper_97_5 = mean(quantile_upper_97_5)
  ) |> 
  rename(Year = year, Month = month, `Angler type` = angler_type_name, `Julian date` = julian_date,
         `lower 2.5%` = quantile_lower_2_5,
         `median 50%` = quantile_median_50,
         `upper 97.5%` = quantile_upper_97_5)


effort_daily_plot <- effort_daily |> 
  ggplot(aes(x = `Julian date`, y = `median 50%`, ymin = `lower 2.5%`, ymax = `upper 97.5%`,
             fill = `Angler type`, color = `Angler type`)) +
  geom_line(linewidth = 0.8) + # Line for median values
  geom_ribbon(data = ~ filter(.x, `Angler type` == "Boat"), alpha = 0.2, color = NA) + # Plot Boat first 
  geom_ribbon(data = ~ filter(.x, `Angler type` == "Bank"), alpha = 0.2, color = NA) + # Plot Bank second
  
  scale_fill_manual(values = c("Bank" = "steelblue", "Boat" = "orange"), labels = c("Bank", "Boat")) +
  scale_color_manual(values = c("Bank" = "steelblue", "Boat" = "orange"), labels = c("Bank", "Boat")) +
  labs(
    x = "Date",
    y = "Estimated angler effort (angler hours)") +
  facet_wrap(~ Year, ncol = 1, scales = "free_x") +
  scale_x_continuous(limits = c(182, 319),
                     breaks = month_ticks$julian_date,     # Use Julian dates for breaks
                     labels = month_ticks$month_label) +
  theme_bw() 

effort_daily_plot

#create BSS estimates table ####

total_catch_pe <- report_estimates$total |> 
  filter(
    model_type == "PE",
    estimate_category == "catch",
    estimate_type == "estimate_sum"
  ) |> 
  select(year, species, life_stage, fin_mark, fate, design_based_estimator = estimate_value)

# bss model diagnostics
# bss_fit <- creel_estimates$stratum |> 
#   filter(
#     estimate_category == "catch",
#     estimate_type %in% c("R_hat", "n_eff")
#   ) |> 
#   group_by(fishery_name, species_name, life_stage_name, fin_mark_desc, fate_name, estimate_type) |> 
#   summarise(
#     mean_value = mean(estimate_value, na.rm = TRUE),
#     .groups = "drop"
#   ) |> 
#   pivot_wider(
#     names_from = estimate_type,
#     values_from = mean_value
#   ) |> 
#   rename_with(~ . |>  str_replace_all("_", " ") |> str_to_sentence()) |> 
#   rename(
#     Species = `Species name`,
#     `Life stage` = `Life stage name`,
#     `Mark status` = `Fin mark desc`,
#     Fate = `Fate name`
#   ) |> 
#   mutate(Year = as.numeric(str_extract(`Fishery name`, "\\d{4}"))) |> 
#   mutate(
#     `Mark status` = case_when(
#       `Mark status` == "AD" ~ "Adipose fin clipped",
#       `Mark status` == "UM" ~ "Unmarked",
#       `Mark status` == "UNK" ~ "Unknown",
#       TRUE ~ `Mark status`
#     ))

total_catch_bss <- report_estimates$total |> 
  filter(
    model_type == "BSS",
    estimate_category %in% c("C_sum", "catch"),
    # estimate_type == "mean"
    estimate_type %in% c("quantile_lower_2_5", "quantile_lower_25", "quantile_median_50", "quantile_upper_75", "quantile_upper_97_5", "mean", "standard_deviation")
  ) |>
  pivot_wider(names_from = estimate_type, values_from = estimate_value) |> 
  select(year, species, life_stage, fin_mark, fate, mean, standard_deviation,quantile_lower_2_5,quantile_median_50, quantile_upper_97_5) |> 
  left_join(total_catch_pe, by = c("year", "species", "life_stage", "fin_mark", "fate")) |> 
  relocate(design_based_estimator, .after = fate) |> 
  arrange(year, species, life_stage, fin_mark, fate) |>
  group_by(year) |> 
  mutate(CV = standard_deviation / mean, .after = "standard_deviation",) |> 
  rename(
    `Mark status` = fin_mark, 
    Mean = mean, 
    SD = standard_deviation,
    `Design-based estimator` = design_based_estimator,
    `2.5%` = "quantile_lower_2_5",
    `50%` = "quantile_median_50",
    `97.5%` = "quantile_upper_97_5",
    Year = year, `Life stage` = life_stage, Fate = fate, Species = species
  )

raw_catch_23 <- raw_data$catch |> 
  group_by(species, life_stage, fin_mark, fate) |> 
  summarise(`Raw catch` = sum(fish_count)) |> 
  rename(
    "Species" = "species",
    "Life stage" = "life_stage",
    "Mark status" = "fin_mark",
    "Fate" = "fate"
  ) |> 
  mutate(Year = as.numeric("2023"))

catch_table_final <- total_catch_bss |> 
  group_by(Year, Species, `Life stage`, `Mark status`, `Fate`) |> 
  left_join(raw_catch_23, by = c("Year", "Species", "Life stage", "Mark status", "Fate")) |> 
  relocate(`Raw catch`, .after = "Fate") |> 
  relocate("Design-based estimator", .after = last_col()) |> 
  mutate(
    `Mark status` = factor(`Mark status`, levels = c("Adipose fin clipped", "Unmarked", "Unknown"))
  ) |> 
  arrange(Species, `Life stage`, `Mark status`, `Fate`) |> 
  ungroup()

# create BSS table and format
gt_catch_table_final <- catch_table_final |> 
  # filter(
  #   `Raw catch` > 30, #low sample catch groups where BSS produces unreliable estimates, use PE
  #   CV < 0.5) |> #filter out other catch groups with highly uncertain estimates
  mutate(`Fishery name` = case_when(
    Year == "2023" ~ "Nisqually salmon 2023"
  )) |> 
  select(-Year) |> 
  gt(
    groupname_col = 'Fishery name'
  ) |> 
  tab_style(
    style = cell_text(indent = px(12)),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      whitespace = "nowrap",  # Disables wrapping
      align = "center"
    ),
    locations = cells_body(columns = `Mark status`)
  ) |>
  fmt_number(columns = c(
    "Mean", "SD", `Raw catch`, `Design-based estimator`,
    `2.5%`, `50%`, `97.5%`), decimals = 0) |>
  fmt_percent(CV, decimals = 1) |> 
  # cols_label(
  # `R hat` = html("R&#770;"),
  # `N eff` = html("n<sub>eff</sub>")
  # ) |>
  cols_width(
    `Design-based estimator` ~ px(120),
  ) |>
  cols_align(`Design-based estimator`, align = "center") |> 
  tab_spanner(
    label = "Bayesian state-space model",
    columns = c("Mean", "SD", "CV", `2.5%`, `50%`, `97.5%`)
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
  )

gt_catch_table_final

gt_catch_table_pe <- raw_catch_23 |> 
  inner_join(
    total_catch_pe,
    by = c(
      "Year" = "year",
      "Species" = "species",
      "Life stage" = "life_stage",
      "Mark status" = "fin_mark",
      "Fate" = "fate"
    )
  ) |> 
  mutate(fishery_name = case_when(
    Year == "2023" ~ "Nisqually salmon 2023"
  )) |> 
  select(-Year) |> 
  rename(`Design-based estimator` = design_based_estimator) |> 
  #remove estimates from BSS table
  anti_join(
    bss_groups, 
    by = c(
      "Species" = "species", "Life stage" = "life_stage", 
      "Mark status" = "fin_mark","Fate" = "fate")
  ) |> 
  gt(groupname_col = 'fishery_name') |>
  fmt_number("Design-based estimator", decimals = 0) |>
  cols_align(columns = c("Raw catch", "Design-based estimator"), align = "center")

gt_catch_table_pe
