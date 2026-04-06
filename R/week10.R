# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import and Cleaning
gss_tbl <- read_sav(file = "../data/GSS2016.sav")
gss_tbl <- gss_tbl %>% 
  zap_missing() %>%
  filter(!is.na(mosthrs)) %>% # Filter out participants who had NA for MOSTHRS
  mutate(hrs1 = NULL, hrs2 = NULL) %>% # Remove HRS1 and HRS2 columns
  select(where(~ mean(is.na(.)) < 0.75)) # Remove any columns where 75% or more participants have missing values

# Visualization
ggplot(gss_tbl, aes(x = mosthrs)) +
  geom_histogram() +
  labs(
    title = "Distribution of Work Hours",
    x = "Most Hours/Week Worked in Past Month",
    y = "Count",
  )