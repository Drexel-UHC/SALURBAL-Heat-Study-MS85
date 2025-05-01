list.files(
  
)

library(tidyverse)


load("cleaned__data_server.rdata")

cleaned__tidy_data = cleaned__tidy_data %>% as_tibble()
cleaned__tidy_data %>% 
  select(-contains("tooltip")) %>% 
  write_csv("cleaned__tidy_data.csv")
