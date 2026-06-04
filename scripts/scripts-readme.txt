prepare_creel_data.R pulls creel data and saves chinook data into cleaned_data/key_dataframes
prepare_creel_data_pinks.R does similar but for pink data.

catch-model-v3.qmd is the current workhorse for exploring models predicting encounters

catch-model-fitting.R runs and saves a series of models as in catch-model-v3, created because the models
  with a random effect of interview took multiple hours
catch-model-evaulations reads in those saved models, and was intended for model diagnostics. Early stages as of 7/17/25

pink-vs-chinook-v3 models chinook and pink phenology. This is in the process of being updated with our new data
and new analysis methods. 