#' Import data and check



{# 0. Setup ------
  
  ## Dependencies
  packages =  c('rstudioapi','tidyverse','janitor','sf','stringi','readxl')
  lapply(packages, require, character.only = TRUE)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ## set directory to current file.
    
  ## Load Helper functions
  source('../util.R')
  
  ## Crosswalk
  xwalk_l1 = read.csv("../../raw data/crosswalks/Sorted SALID.CSV") %>% as_tibble() %>% 
    clean_names() %>% 
    select(salid1=nsalid1 , city = city_name,country   ) %>% 
    sanitize_city_names() %>% 
    mutate_all(~as.character(.x))
  save(xwalk_l1, file = "../../Clean/xwalk_l1.rdata")
  
  xwalk_l1_server = read.sas7bdat("../../raw data/crosswalks/l1_salurbal_5_27_21.sas7bdat") %>% 
    as_tibble() %>% 
    select(salid1 = SALID1, country = COUNTRY_NAME ) %>% 
    distinct()%>% 
    mutate_all(~as.character(.x))
  save(xwalk_l1_server, file = "../../Clean/xwalk_l1_server.rdata")
  
}



{# 1. Josiah Data------
  df_JK = read_excel("../../raw data/heat_app_rr_5-3-22.xlsx") %>% clean_names() %>% 
    mutate(age = age %>% recode("ALLAGES"="Crude",
                                'AGE65PLUS'="65+"))
}

{# 2. Jeff Data ------ 
  
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
  save(df_import_allAges, df_import_65plus, df_JK,file = "imported_data.rdata")
  
}

{# 3. Figure Checks ------ 
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

