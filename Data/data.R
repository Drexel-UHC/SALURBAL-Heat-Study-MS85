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
  cleaned__tidy_data = df_import_65plus %>% 
    mutate(grp = "65+") %>% 
    bind_rows(df_import_allAges %>% mutate(grp = "Crude")) %>% 
    mutate_all(~as.character(.x)) %>% 
    pivot_longer(-c(city, country, salid1, grp), names_to = 'var') %>% 
    arrange(var, grp, salid1)
  
  
}

save(cleaned__tidy_data,
     df__l1_centroids,
     file = "../App/R/Data/cleaned__data.rdata")
