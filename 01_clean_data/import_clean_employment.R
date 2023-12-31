# Import and Clean employment data ----
# Number of Employed in 10 Counties of NH
# **** ----


# Load packages
library(tidyverse)

# NH County Employment ----
## Import data ----
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

## Combine into one table ----
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

## Clean data ----
data_tbl <- empl_all %>%
    select(county, date, empl) %>% 
    
    # Create a categorical variable for southern and northern counties
    mutate(region = if_else(county %in% c("Belknap", "Carroll", "Coos", "Grafton", "Sullivan"), "Northern", "Southern")) %>%
    
    # Reorder counties in the order of region
    mutate(county = factor(county, 
                           levels = c("Belknap", "Carroll", "Coos", "Grafton", "Sullivan",
                                      "Cheshire", "Hillsborough", "Merrimack", "Rockingham", "Strafford")))

write_rds(data_tbl, "00_data/data_tbl.rds")

# External Predictors ----

## MA employment ----
empl_MA      <- tidyquant::tq_get("LAUST250000000000005", get = "economic.data", from = "1990-01-01")

# Clean
empl_MA_clean <- empl_MA %>% select(date, empl_MA = price)
## NH unemployment initial claims ----
claims_NH    <- tidyquant::tq_get("NHICLAIMS", get = "economic.data", from = "1990-01-01")

# Convert to monthly from weekly
claims_NH_clean <- claims_NH %>%
    summarise_by_time(date, .by = "month", claims_NH = sum(price))

## Combine into one table
external_var_tbl <- empl_MA_clean %>% 
    left_join(claims_NH_clean)

write_rds(external_var_tbl, "00_data/external_var_tbl.rds")
