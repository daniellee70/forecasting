# Explore employment ----
# Number of Employed in 10 Counties of NH
# **** ----

# OBJECTIVE ----
# - Check the distribution of county employment 
# - variability, correlation, seasonality
# **** ----


# LIBRARIES ----

library(tidyverse)
library(timetk)
library(tidyquant)
library(patchwork)

# DATA -----
data_tbl <- read_rds("00_data/data_tbl.rds")
external_var_tbl <- read_rds("00_data/external_var_tbl.rds")

# One County ----

data_tbl %>%
    filter(county == "Grafton") %>%
    plot_time_series(.date_var = date, .value = empl)

# By Month ----

data_tbl %>%
    plot_time_series(.date_var   = date, 
                     .value      = empl, 
                     .color_var  = year(date), 
                     .smooth     = F,
                     .facet_vars = county, 
                     .facet_ncol = 2, 
                     .facet_dir = "v", 
                     .legend_show = FALSE, .interactive = FALSE,
                     
                     # Customization
                     .title = "Number of Employed in NH Counties", 
                     .y_lab = "Number of People", 
                     .color_lab = "Year")

# By Decade ----
# Box Plots

empl_dec_box_fig <- data_tbl %>%
    plot_time_series_boxplot(date, empl, 
                             .facet_vars  = county, .color_var = region, 
                             .facet_ncol  = 2, 
                             .facet_dir = "v", 
                             .smooth      = FALSE, 
                             .interactive = FALSE, 
                             .period      = "10 years",
                             
                             # Customization
                             .title = "Monthly Employment in NH Counties by Decades", 
                             .y_lab = "Monthly Number of the Employed", 
                             .x_lab = NULL, 
                             .color_lab = "Counties") +
    
    scale_y_continuous(labels = scales::number_format(scale = 0.001, suffix = "K"))

empl_dec_box_fig

write_rds(empl_dec_box_fig, "00_fig/empl_dec_box_fig.rds")

# Seasonality ----

## By Region ----
empl_seasonal_region_fig <- data_tbl %>%
    
    group_by(region) %>%
    summarise_by_time(date, 
                      .by = "month",
                      empl = sum(empl)) %>%

    plot_seasonal_diagnostics(date, empl, 
                              .interactive = FALSE, 
                              .feature_set = "auto") + 
    
    theme(axis.text.x = element_text(angle = 35)) +
    scale_y_continuous(labels = scales::number_format(scale = 0.001, suffix = "K"))

empl_seasonal_region_fig

write_rds(empl_seasonal_region_fig, "00_fig/empl_seasonal_region_fig.rds")

## By County ----

# northern counties with seasonality
empl_seasonal_north_fig <- data_tbl %>%
    filter(region == "Northern") %>%
    group_by(county) %>%
    plot_seasonal_diagnostics(date, empl, 
                              .interactive = FALSE, 
                              .feature_set = "month.lbl", 
                              .title = "Northern Counties") + 
    
    theme(axis.text.x = element_text(angle = 35)) +
    scale_y_continuous(labels = scales::number_format(scale = 0.001, suffix = "K"))

empl_seasonal_north_fig

write_rds(empl_seasonal_north_fig, "00_fig/empl_seasonal_north_fig.rds")

# southern counties with seasonality
empl_seasonal_south_fig <- data_tbl %>%
    filter(region == "Southern") %>%
    group_by(county) %>%
    plot_seasonal_diagnostics(date, empl, 
                              .interactive = FALSE, 
                              .feature_set = "month.lbl", 
                              .title = "Southern Counties") + 
    
    theme(axis.text.x = element_text(angle = 35)) +
    scale_y_continuous(labels = scales::number_format(scale = 0.001, suffix = "K")) 

empl_seasonal_south_fig

write_rds(empl_seasonal_south_fig, "00_fig/empl_seasonal_south_fig.rds")

empl_seasonal_north_fig / empl_seasonal_south_fig


# Regression Plots ----
# Quickly assess key features that are correlated to employment

data_tbl %>%
    group_by(county) %>%
    plot_time_series_regression(date,  
                                .formula = empl ~ as.numeric(date) + month(date, label = T) + year(date), 
                                .facet_ncol = 2, 
                                .show_summary = TRUE)


# STL Diagnostics ----
data_tbl %>%
    
    group_by(county) %>%
    
    plot_stl_diagnostics(date, empl, 
                         .frequency = "auto", .trend = "auto", 
                         .feature_set = c("observed", "season", "trend", "remainder"),
                         .interactive = FALSE)

# ACF Diagnostics ----
## ACF plots ----
data_tbl %>%
    
    filter(region == "Northern") %>%
    
    group_by(county) %>%
    plot_acf_diagnostics(date, empl, .lags = "24 months")


data_tbl %>%
    
    filter(region == "Southern") %>%
    
    group_by(county) %>%
    plot_acf_diagnostics(date, empl, .lags = "24 months")

## CCF plots ---- 
# Check correlation with MA
empl_MA      <- tidyquant::tq_get("LAUST250000000000005", get = "economic.data", from = "1990-01-01")

# NH unemployment initial claims
claims_NH    <- tidyquant::tq_get("NHICLAIMS", get = "economic.data", from = "1990-01-01")

### Northern Counties ----
corr_with_ma_north_fig <- data_tbl %>%
    
    filter(region == "Northern") %>%
    
    # Add MA empl 
    left_join(empl_MA %>%
                  select(date, empl_MA = price)) %>%
    
    group_by(county) %>%
    plot_acf_diagnostics(
        date, empl,        # ACF & PACF
        .ccf_vars    = empl_MA,   # CCFs
        .show_ccf_vars_only = TRUE,
        .facet_ncol = 2,
        .lags        = "24 months",    # 24 months of monthly lags
        .interactive = FALSE, 
        .title = "Northern Counties" 
    )

corr_with_ma_north_fig

write_rds(corr_with_ma_north_fig, "00_fig/corr_with_ma_north_fig.rds")

### Southern Counties ----
corr_with_ma_south_fig <- data_tbl %>%
    
    filter(region == "Southern") %>%
    
    # Add MA empl 
    left_join(empl_MA %>%
                  select(date, empl_MA = price)) %>%
    
    group_by(county) %>%
    plot_acf_diagnostics(
        date, empl,        # ACF & PACF
        .ccf_vars    = empl_MA,   # CCFs
        .show_ccf_vars_only = TRUE,
        .facet_ncol = 2,
        .lags        = "24 months",    # 24 months of monthly lags
        .interactive = FALSE, 
        .title = "Southern Counties" 
    )

corr_with_ma_south_fig

write_rds(corr_with_ma_south_fig, "00_fig/corr_with_ma_south_fig.rds")

patchwork <- corr_with_ma_north_fig / corr_with_ma_south_fig

patchwork  + plot_annotation(
    title = 'Variations in Cross Correlations in Monthly Employment',
    subtitle = 'between New Hampshire Counties and Massachusetts',
    caption = 'Data: U.S. Bureau of Labor Statistics, Employed Persons'
)
