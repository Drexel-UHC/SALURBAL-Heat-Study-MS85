{# 0. Setup ------
  
  ## Dependencies
  library(tidyverse)  
  library(janitor)
  library(sas7bdat)
  library(stringi)

  ## Load Helper functions
  source('R/util.R')
  
  ## Load Centroids
  load("../../SALURBAL Dashboard Portal/Data/Clean/Spatial/df__l1_centroids.rdata ")
  
  ## Load processed data
  load("R/Import Data/imported_data.rdata")
}


{  # 1. Process data for dashboard ------
  ## Tidy 
  cleaned__tidy_data1 = df_import_65plus %>% 
    mutate(grp = "65+") %>% 
    bind_rows(df_import_allAges %>% mutate(grp = "Crude")) %>% 
    mutate_all(~as.character(.x)) %>% 
    rename(climate = level_1_climate,
           age = grp) %>% 
    pivot_longer(-c(city, salid1, country,age, climate), names_to = 'metric') %>% 
    arrange(metric, country,age,climate, salid1) %>% 
    filter(metric%in%c('average_temperature')) %>% 
    mutate(metric = metric %>% 
             recode("average_temperature"="Mean Temperature"),
           value = as.numeric(value) %>% 
             round(3)) %>% 
    left_join(df__l1_centroids %>% select(salid1, long,lat), by = 'salid1') 
  ### Simulate rr
  
  dfa = df_import_65plus %>% 
    mutate(grp = "65+") %>% 
    bind_rows(df_import_allAges %>% mutate(grp = "Crude")) %>% 
    mutate_all(~as.character(.x)) %>% 
    rename(climate = level_1_climate,
           age = grp) %>% 
    pivot_longer(-c(city, salid1, country,age, climate), names_to = 'metric') %>% 
    arrange(metric, country,age,climate, salid1) %>% 
    filter(metric%in%c('average_temperature')) %>% 
    rowwise() %>% 
    mutate(metric = 'RR at P99',
           value = rnorm(1,1,0.1) %>%  round(3)) %>% 
    ungroup() %>% 
    left_join(df__l1_centroids %>% select(salid1, long,lat), by = 'salid1') 
  
  
  cleaned__tidy_data = cleaned__tidy_data1 %>% 
    bind_rows(dfa) 
  
}

save(cleaned__tidy_data,
     file = "../App/R/Data/cleaned__data.rdata")
