# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES TRANSFORMATIONS -----

# GOAL ----
# - Exposure to Common Time Series Transformations

# OBJECTIVES ----
# - Variance Reduction - Log, Log1P, Box Cox
# - Rolling & Smoothing
# - Range Reduction - Normalization & Standardization
# - Imputation & Outliers
# - Lags & Differencing
# - Fourier Series
# - Confined Interval Forecasting



# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)

# DATA ----

# empl_Belknap      <- tidyquant::tq_get("LAUCN330010000000005", get = "economic.data", from = "1990-01-01")
# empl_Carroll      <- tidyquant::tq_get("LAUCN330030000000005", get = "economic.data", from = "1990-01-01")
# empl_Cheshire     <- tidyquant::tq_get("LAUCN330050000000005", get = "economic.data", from = "1990-01-01")
# empl_Coos         <- tidyquant::tq_get("LAUCN330070000000005", get = "economic.data", from = "1990-01-01")
# empl_Grafton      <- tidyquant::tq_get("LAUCN330090000000005", get = "economic.data", from = "1990-01-01")
# empl_Hillsborough <- tidyquant::tq_get("LAUCN330110000000005", get = "economic.data", from = "1990-01-01")
# empl_Merrimack    <- tidyquant::tq_get("LAUCN330130000000005", get = "economic.data", from = "1990-01-01")
# empl_Rockingham   <- tidyquant::tq_get("LAUCN330150000000005", get = "economic.data", from = "1990-01-01")
# empl_Strafford    <- tidyquant::tq_get("LAUCN330170000000005", get = "economic.data", from = "1990-01-01")
# empl_Sullivan     <- tidyquant::tq_get("LAUCN330190000000005", get = "economic.data", from = "1990-01-01")
# 
# empl_all <- list(Belknap = empl_Belknap, 
#                   Carroll = empl_Carroll,
#                   Cheshire = empl_Cheshire,
#                   Coos = empl_Coos,
#                   Grafton = empl_Grafton,
#                   Hillsborough = empl_Hillsborough,
#                   Merrimack = empl_Merrimack,
#                   Rockingham = empl_Rockingham,
#                   Strafford = empl_Strafford, 
#                   Sullivan = empl_Sullivan) %>% 
#     enframe() %>% 
#     unnest(value) %>% 
#     select(county = name, date, empl = price)
# 
# empl_all
# 
# write_rds(empl_all, "00_data/empl_all.rds")

empl_all <- read_rds("00_data/empl_all.rds")

data_tbl <- empl_all %>%
    select(county, date, empl)

data_Belknap_tbl <- data_tbl %>%
    filter(county == "Belknap") %>%
    select(date, empl)

# 1.0 VARIANCE REDUCTION ----

# * Log & Log Plus 1----

# No transformation

data_Belknap_tbl %>%
    plot_time_series(date, empl)

data_Belknap_tbl %>%
    plot_time_series_regression(
        .date_var = date,
        .formula = empl ~ as.numeric(date) +
            month(date, label = TRUE),
        .show_summary = TRUE
    )

# Log - Show Error 
data_Belknap_tbl %>%
    plot_time_series(date, log(empl))

# Log Plus 1
data_Belknap_tbl %>%
    plot_time_series(date, log1p(empl))

# Inversion
data_Belknap_tbl %>%
    plot_time_series(date, log1p(empl) %>% expm1())

# Benefit
data_tbl %>%
    plot_time_series_regression(
        .date_var = date,
        .formula = log1p(empl) ~ as.numeric(date) +
            wday(date, label = TRUE) +
            month(date, label = TRUE),
        .show_summary = TRUE
    )

# There is no benefit of log transformation in this case.

# Groups
data_tbl %>%
    plot_time_series(date, empl, .facet_vars = county)

# Inversion
data_tbl %>%
    plot_time_series(date, expm1(log1p(empl)), .facet_vars = county)

# 2.0 ROLLING & SMOOTHING ----
# - Common time series operations to visualize trend
# - A simple transformation that can help create improve features 
# - Can help with outlier-effect reduction & trend detection
# - Note: Businesses often use a rolling average as a forecasting technique 
#   - A rolling average forecast is usually sub-optimal (good opportunity for you!)

# 3.0 RANGE REDUCTION ----
# - Used in visualization to overlay series
# - Used in ML for models that are affected by feature magnitude (e.g. linear regression)



# 4.0 IMPUTING & OUTLIER CLEANING ----
# - Imputation helps with filling gaps (if needed)
# - Outlier removal helps linear regression detect trend and reduces high leverage points
# WARNING: Make sure you check outliers against events 
# - usually there is a reason for large values


# * Cleaning (Imputation + Outlier Removal) ----
data_Belknap_tbl %>%
    plot_anomaly_diagnostics(date, empl)

data_Belknap_cleaned_tbl <- data_Belknap_tbl %>%
    mutate(empl_cleaned= ts_clean_vec(empl, period = 12)) 

data_Belknap_cleaned_tbl %>%
    pivot_longer(-date) %>%
    plot_time_series(date, value, name, .smooth = F)


# Outlier Effect - Before Cleaning
data_Belknap_cleaned_tbl %>%
    plot_time_series_regression(
        date, 
        .formula = empl ~ as.numeric(date) +
            wday(date, label = TRUE) +
            month(date, label = TRUE),
        .show_summary = TRUE
    )


# Outlier Effect - After Cleaning
data_Belknap_cleaned_tbl %>%
    plot_time_series_regression(
        date, 
        .formula = empl_cleaned ~ as.numeric(date) +
            wday(date, label = TRUE) +
            month(date, label = TRUE),
        .show_summary = TRUE
    )



# 5.0 LAGS & DIFFERENCING -----
# - Used to go from growth to change
# - Makes a series "stationary" (potentially)
# - MOST IMPORTANT - Can possibly use lagged variables in a model, if lags are correlated & 

# * Lags ----
# - Often used for feature engineering
# - Autocorrelation
# - 

data_Belknap_cleaned_tbl %>%
    mutate(empl_lag_1 = lag_vec(empl, lag = 1))

data_Belknap_cleaned_tbl %>%
    plot_acf_diagnostics(date, empl)

data_Belknap_cleaned_tbl %>%
    tk_augment_lags(.value = empl, .lags = c(1, 2, 12)) %>%
    drop_na() %>%
    plot_time_series_regression(
        date, 
        .formula = empl ~ empl_lag1 + empl_lag41,
        .show_summary = TRUE
    )


# * Differencing ----
# - Makes a series "stationary"
# - Used to get change
#   - Stock price changes
#   - Cumulative Revenue to change by day
#   - Total subs to change by day

# Cumulative Sum & Differencing

data_Belknap_tbl %>%

    mutate(empl_diff_1 = diff_vec(empl, lag = 1, difference = 1)) %>%
    mutate(empl_diff_2 = diff_vec(empl, lag = 1, difference = 2)) %>%
    
    pivot_longer(-date) %>%
    
    group_by(name) %>%
    
    plot_time_series(date, value, name, .smooth = FALSE)



# Comparing Differences 
initial_value <- 24431

data_Belknap_diff_tbl <- data_Belknap_tbl %>%
    mutate(empl = diff_vec(empl)) 

# Inversion 
data_Belknap_diff_tbl %>%
    mutate(empl = diff_inv_vec(empl, initial_values = 24431))



# 6.0 FOURIER SERIES ----
# - Useful for incorporating seasonality & autocorrelation
# - BENEFIT: Don't need a lag, just need a frequency (based on your time index)

# * Vector (Single Fourier) ----

data_tbl %>%
    mutate(sin14_k1 = fourier_vec(date, period = 14, K = 1, type = "sin")) %>%
    mutate(cos14_k1 = fourier_vec(date, period = 14, K = 1, type = "cos")) %>%
    select(-empl) %>%
    pivot_longer(matches("(cos)|(sin)")) %>%
    plot_time_series(date, value, name, .smooth = FALSE)


# * Augmenting (Multiple Fourier Series) ----
data_tbl %>%
    tk_augment_fourier(date, .periods = c(14, 30, 90, 365), .K = 2) %>%
    
    plot_time_series_regression(
        date,
        .formula = log1p(empl) ~ as.numeric(date) + . - date,
        .show_summary = TRUE
    )



# 7.0 CONFINED INTERVAL FORECASTING ----
# - Showcase: log_interval_vec()
# - Transformation used to confine forecasts to a max/min interval

# * Transformation ----

1:10

values_transformed_vec <- log_interval_vec(1:10, limit_lower = 0, limit_upper = 15)

values_transformed_vec %>% log_interval_inv_vec(limit_lower = 0, limit_upper = 15)

new_values_transformed_vec <- c(values_transformed_vec, c(.75, 1.5, 2.0, 10.0,)) 

new_values_transformed_vec %>% plot()

new_values_transformed_vec %>% log_interval_inv_vec(0, 15) %>% plot()

# * Data ----

limit_lower <- 0
limit_upper <- 3650.8
offset      <- 1

data_tbl %>%
    plot_time_series(date, log_interval_vec(empl, 
                                                  limit_lower = limit_lower, 
                                                  limit_upper = limit_upper,
                                                  offset      = offset))



# * Apply Transformation ----

fourier_periods <- c(6, 14, 30, 90, 365)
fourier_order   <- 5

data_transformed_tbl <- data_tbl %>%
    mutate(empl_trans = log_interval_vec(empl, 
                                           limit_lower = limit_lower, 
                                           limit_upper = limit_upper, 
                                           offset = offset)) %>%
    tk_augment_fourier(date, .periods = fourier_periods, .K = fourier_order) %>%
    select(-empl)

data_transformed_tbl %>% glimpse()

# * Model ----

model_formula <- as.formula(
    empl_trans ~ as.numeric(date) +
        wday(date, label = TRUE) +
        month(date, label = TRUE) +
        . - date)
model_formula

data_transformed_tbl %>%
    plot_time_series_regression(
        .date_var = date,
        .formula  = model_formula,
        .show_summary = TRUE
    )

model_fit_lm <- lm(formula = model_formula, data = data_transformed_tbl)

summary(model_fit_lm)

# * Create Future Data ----

data_transformed_tbl

future_tbl <- data_transformed_tbl %>%
    future_frame(.length_out = "6 months") %>%
    tk_augment_fourier(date, .periods = fourier_periods, .K = fourier_order)

future_tbl 

# * Predict ----

predictions <- predict(model_fit_lm, newdata = future_tbl) %>% as.vector()
predictions

# * Combine data ----

conf_interval <- 0.95
residuals     <- model_fit_lm$residuals %>% as.vector()

alpha <- (1-conf_interval) / 2
1 - alpha

qnorm(alpha)
qnorm(1-alpha)

abs_margin_error <- abs(qnorm(alpha) * sd(residuals))

forecast_tbl <- data_transformed_tbl %>%
    select(date, empl_trans) %>%
    add_column(type = "actual") %>%
    bind_rows(
        future_tbl %>%
            select(date) %>%
            mutate(
                empl_trans = predictions,
                type        = "prediction"
            ) %>%
            mutate(
                conf_lo = empl_trans - abs_margin_error,
                conf_hi = empl_trans + abs_margin_error
            )
    )

forecast_tbl 


forecast_tbl %>%
    pivot_longer(cols = c(empl_trans, conf_lo, conf_hi)) %>%
    plot_time_series(date, value, .color_var = name, .smooth = FALSE)

# * Invert Transformation ----

forecast_tbl %>%
    pivot_longer(cols = c(empl_trans, conf_lo, conf_hi)) %>%
    plot_time_series(
        date, 
        log_interval_inv_vec(
            x = value, 
            limit_lower = limit_lower,
            limit_upper = limit_upper,
            offset      = offset
        ),
        .color_var = name,
        .smooth = FALSE
    )
