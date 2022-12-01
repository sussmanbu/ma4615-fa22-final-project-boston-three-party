library(tidyverse)

co2_data <- read_csv(here::here("dataset", "CSV_fixed_climate_data.csv"))

## CLEAN the data
co2_data_clean <- co2_data

write_csv(co2_data_clean, file = here::here("dataset", "clean_co2.csv"))

save(co2_data_clean, file = here::here("dataset/clean_co2.RData"))