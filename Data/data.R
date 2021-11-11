{# 0. Setup ------
  
  ## Dependencies
  library(tidyverse)  
  library(janitor)
  library(sas7bdat)
  library(stringi)
  library(glue)
  
  ## Load Helper functions
  source('R/util.R')
  
  ## Load Centroids
  load("../../SALURBAL Dashboard Portal/Data/Clean/Spatial/df__l1_centroids.rdata ")
  
  ## Load processed data
  load("R/Import Data/imported_data.rdata")
}


{  # 1. Process data for dashboard ------
  ## Process Jeff data  
  cleaned_data_jeff = df_import_65plus %>% 
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
             round(3)) 
  ### process Josiah Data
  cleaned_data_josiah = df_JK %>% 
    mutate_all(~as.character(.x)) %>% 
    pivot_longer(cols = c("rr_99vs95_per1c",  "rrcat_p99"  ,   "rr_1vs5_per1c" ,  "rrcat_p1")) %>% 
    mutate(type = ifelse(str_detect(name, 'per1c'),"value",'cat'),
           metric = ifelse(str_detect(name, '99'),"RR of heat-related mortality","RR of cold-related mortality")) %>% 
    select(-name) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    mutate(value = as.numeric(value) %>% round(3),
           cat = factor(cat, levels =   c("<= 1.000" ,"1.001 - 1.049", "1.050 - 1.099", "1.100 - 1.149", "1.150 +"  )))

  
  
  ## Final processing
  cleaned__tidy_data = cleaned_data_jeff %>% 
    bind_rows(cleaned_data_josiah) %>% 
    left_join(df__l1_centroids %>% select(salid1, long,lat), by = 'salid1') %>% 
    mutate(
      tooltip__map = glue(
        '<span class="map-tooltip-header">{city}</span><br />
        SALID1: {salid1}<br />
        {metric}: {value}'),
      tooltip__beeswarmPlotly = glue(
        '<b>{city}</b>
        Country: {country}
        Value: {value}'
      )) 
  
}

save(cleaned__tidy_data,
     file = "../App/R/Data/cleaned__data.rdata")
