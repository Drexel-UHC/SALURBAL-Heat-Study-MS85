#' Import data and check
#' Issues:
#' @1: data doesnt have salid. so we have to name match. There are 
#'     are four cities that could not be linked
#' @2: There is a city (Leon, 205103) which figure PNG is not available 


{# 0. Setup ------
  
  ## Dependencies
  library(tidyverse)  
  library(janitor)
  library(sas7bdat)
  library(stringi)
  
  ## Load Helper functions
  source('../util.R')
  
  ## Crosswalk
  xwalk_l1 = read.csv("../../raw data/crosswalks/Sorted SALID.CSV") %>% as_tibble() %>% 
    clean_names() %>% 
    select(salid1=nsalid1 , city = city_name,country   ) %>% 
    sanitize_city_names()
}



{# 1. Data ------ 
  
  ## All Ages
  df_import_allAges_raw = read.csv('../../raw data/App Data All Ages.csv') %>%  
    sanitize_city_names()%>% 
    left_join(xwalk_l1)
  df_import_allAges_raw %>% filter(is.na(salid1)) %>% pull(city)  
  df_import_allAges = df_import_allAges_raw %>% filter(!is.na(salid1))
  
  ##  65+ 
  df_import_65plus = read.csv('../../raw data/App Data 65 Plus.csv') %>%  
    sanitize_city_names()%>% 
    left_join(xwalk_l1) %>% filter(!is.na(salid1))
  
  ## Save
  save(df_import_allAges, df_import_65plus, file = "imported_data.rdata")
  
}

{# 2. Figure Checks ------ 
  ## All Ages
  salid_allAges = list.files(path = "../../raw data/figures/", pattern ='ALLAGES') %>% 
    parse_number() %>% as.character()
  df_import_allAges %>%  filter(!salid1%in%salid_allAges) %>% select(city, salid1)

  ## 65+
  salid_65plus = list.files(path = "../../raw data/figures/", pattern ='PLUS') %>%
    str_sub(6,-1L) %>% 
    parse_number() %>% as.character()
  df_import_65plus %>%  filter(!salid1%in%salid_65plus) %>% select(city, salid1)
  ## Missing 65+ figure fore Leon (205103)
  }

