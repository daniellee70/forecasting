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
data_tbl <- read_rds("00_data/data_tbl.rds")

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

# Log 
data_Belknap_tbl %>%
    plot_time_series(date, log(empl))

# Inversion
data_Belknap_tbl %>%
    plot_time_series(date, log(empl) %>% exp())

# Benefit
data_Belknap_tbl %>%
    plot_time_series_regression(
        .date_var = date,
        .formula = log(empl) ~ as.numeric(date) +
            month(date, label = TRUE),
        .show_summary = TRUE
    )

# There is no benefit of log transformation in this case.

# Groups
data_tbl %>%
    plot_time_series(date, empl, .facet_vars = county)

# Inversion
data_tbl %>%
    plot_time_series(date, exp(log(empl)), .facet_vars = county)

# 2.0 ROLLING & SMOOTHING ----
# - Common time series operations to visualize trend
# - A simple transformation that can help create improve features 
# - Can help with outlier-effect reduction & trend detection
# - Note: Businesses often use a rolling average as a forecasting technique 
#   - A rolling average forecast is usually sub-optimal (good opportunity for you!)

# * Sliding / Rolling Functions ----
data_Belknap_tbl %>%
    
    mutate(empl_roll = slidify_vec(empl, 
                                    .f = mean, 
                                    .period = 12, 
                                    .align = "center", 
                                    .partial = TRUE)) %>%
    
    pivot_longer(-date) %>%
    plot_time_series(date, value, .color_var = name, .smooth = FALSE)


# * LOESS smoother ----

data_Belknap_tbl %>%
    
    mutate(empl_smooth = smooth_vec(empl, 
                                     period = 12,
                                     # span = 0.75
                                     degree = 0)) %>%
    
    pivot_longer(-date) %>%
    plot_time_series(date, value, .color_var = name, .smooth = FALSE)

# * Rolling Correlations ----
# - Identify changing relationships

cor(1:10, seq(0, 20, length.out = 10))

rolling_cor_12 <- slidify(
    .f = ~ cor(.x, .y, use = "pairwise.complete.obs"),
    .period  = 12,
    .align   = "center",
    .partial = FALSE
)

data_Belknap_tbl %>%
    
    # Add MA empl
    left_join(external_var_tbl %>%
                  select(date, empl_MA)) %>%
    
    # Get rolling Corr
    mutate(rolling_cor_empl = rolling_cor_12(empl, empl_MA)) %>%
    # mutate(dateHour = ymd_h(dateHour)) %>%
    # select(-sessions) %>%
    pivot_longer(-date) %>%
    group_by(name) %>%
    plot_time_series(date, value)


# * Problem with Moving Avg Forecasting ----

data_Belknap_tbl %>%
    mutate(
        mavg_12 = slidify_vec(empl, .f = ~ mean(.x, na.rm = TRUE), .period = 12, .align = "right")
    ) %>%
    bind_rows(
        future_frame(., .length_out = 12)
    ) %>%
    fill(mavg_12, .direction = "down") %>%
    pivot_longer(-date) %>%
    plot_time_series(date, value, name, .smooth = F)




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
