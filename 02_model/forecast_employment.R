# Forecast employment ----
# Number of Employed in 10 Counties of NH
# **** ----

# BUSINESS OBJECTIVE ----
# - Forecast county employment 
# - Predict next 12-months
# **** ----


# LIBRARIES ----

library(tidymodels)
library(modeltime.h2o)
library(tidyverse)
library(timetk)
library(tidyquant)

# DATA -----

empl_Belknap      <- tidyquant::tq_get("LAUCN330010000000005", get = "economic.data", from = "1990-01-01")
empl_Carroll      <- tidyquant::tq_get("LAUCN330030000000005", get = "economic.data", from = "1990-01-01")
empl_Cheshire     <- tidyquant::tq_get("LAUCN330050000000005", get = "economic.data", from = "1990-01-01")
empl_Coos         <- tidyquant::tq_get("LAUCN330070000000005", get = "economic.data", from = "1990-01-01")
empl_Grafton      <- tidyquant::tq_get("LAUCN330090000000005", get = "economic.data", from = "1990-01-01")
empl_Hillsborough <- tidyquant::tq_get("LAUCN330110000000005", get = "economic.data", from = "1990-01-01")
empl_Merrimack    <- tidyquant::tq_get("LAUCN330130000000005", get = "economic.data", from = "1990-01-01")
empl_Rockingham   <- tidyquant::tq_get("LAUCN330150000000005", get = "economic.data", from = "1990-01-01")
empl_Strafford    <- tidyquant::tq_get("LAUCN330170000000005", get = "economic.data", from = "1990-01-01")
empl_Sullivan     <- tidyquant::tq_get("LAUCN330190000000005", get = "economic.data", from = "1990-01-01")

empl_all <- list(Belknap = empl_Belknap, 
                  Carroll = empl_Carroll,
                  Cheshire = empl_Cheshire,
                  Coos = empl_Coos,
                  Grafton = empl_Grafton,
                  Hillsborough = empl_Hillsborough,
                  Merrimack = empl_Merrimack,
                  Rockingham = empl_Rockingham,
                  Strafford = empl_Strafford, 
                  Sullivan = empl_Sullivan) %>% 
    enframe() %>% 
    unnest(value) %>% 
    select(county = name, date, empl = price)

empl_all

data_tbl <- empl_all %>%
    select(county, date, empl)

# * Time Plot ----
data_tbl %>% 
    group_by(county) %>% 
    plot_time_series(
        .date_var    = date,
        .value       = empl,
        .facet_ncol  = 2,
        .smooth      = TRUE,
        # .smooth_period = "2 months",
        .interactive = TRUE
    )

# * Seasonality Plot ----
counties <- unique(data_tbl$county)

data_tbl %>% 
    filter(county == counties[1]) %>%
    plot_seasonal_diagnostics(
        .date_var    = date,
        .value       = log(empl)
    )

# TRAIN / TEST SPLITS ---- 

FORECAST_HORIZON <- 12

splits <- time_series_split(data_tbl, assess = FORECAST_HORIZON, cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, empl)

# PREPROCESSING ----

recipe_spec <- recipe(empl ~ ., data = training(splits)) %>%
    step_timeseries_signature(date) %>%
    step_normalize(date_index.num, starts_with("date_year")) 

recipe_spec %>% prep() %>% juice() %>% glimpse()

# MODELING ----

# Initialize H2O
h2o.init(
    nthreads = -1,
    ip       = 'localhost',
    port     = 54321
)

# Optional (Turn off progress)
h2o.no_progress()

# * Model Specification ----

model_spec_h2o <- automl_reg(mode = 'regression') %>%
    set_engine(
        engine                     = 'h2o',
        max_runtime_secs           = 30, 
        max_runtime_secs_per_model = 10,
        max_models                 = 30,
        nfolds                     = 5,
        exclude_algos              = c("DeepLearning"),
        verbosity                  = NULL,
        seed                       = 786
    ) 

model_spec_h2o

# * Fitting ----
#   - This step will take some time depending on your Model Specification selections

wflw_fit_h2o <- workflow() %>%
    add_model(model_spec_h2o) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits))

wflw_fit_h2o

# H2O AUTOML OBJECTS -----

# * H2O AutoML Leaderboard ----

wflw_fit_h2o %>% automl_leaderboard()

# Saving / Loading Models ----

wflw_fit_h2o %>%
    automl_update_model('StackedEnsemble_AllModels_1_AutoML_1_20230916_82507') %>%
    save_h2o_model(path = 'h2o_models/StackedEnsemble_AllModels_1_AutoML_1_20230916_82507')

load_h2o_model("h2o_models/StackedEnsemble_AllModels_1_AutoML_1_20230916_82507/")

# FORECASTING ----

# * Modeltime Table ----
modeltime_tbl <- modeltime_table(
    wflw_fit_h2o,
    wflw_fit_h2o %>%
        automl_update_model('StackedEnsemble_AllModels_1_AutoML_1_20230916_82507')
) 

modeltime_tbl

# * Calibrate ----

calibration_tbl <- modeltime_tbl %>%
    modeltime_calibrate(testing(splits)) 

calibration_tbl %>% modeltime_accuracy() %>% table_modeltime_accuracy()

# * Forecasting ----

calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = data_tbl,
        keep_data   = TRUE
    ) %>%
    group_by(county) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 2, 
        .interactive = TRUE
    )

# * Refitting ----
#   - Working with Erin LeDell to provide option
#     for retraining specific models

refit_tbl <- calibration_tbl %>%
    modeltime_refit(data_tbl)

# * Future Forecast ----

future_tbl <- testing(splits) %>%
    group_by(county) %>%
    future_frame(date, .length_out = 12) %>%
    ungroup()

refit_tbl %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_tbl,
        keep_data   = TRUE
    ) %>%
    group_by(county) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 2, 
        .interactive = TRUE
    )





