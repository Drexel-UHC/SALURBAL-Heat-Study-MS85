library(tidyverse)  
library(janitor)
library(sas7bdat)
library(stringi)


df_allAges = read.csv('../../raw data/App Data All Ages.csv') %>% as_tibble() %>% select(mean)
df_65plus = read.csv('../../raw data/App Data 65 Plus.csv') %>% as_tibble()


