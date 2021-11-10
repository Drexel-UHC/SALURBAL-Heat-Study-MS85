sanitize_city_names = function(dfTmp){
  dfTmp %>% 
    as_tibble() %>% 
    clean_names()  %>% 
    mutate(city = stri_trans_general(str = city, 
                                     id = "Latin-ASCII"),
           ## Remove ( )
           index = ifelse(str_detect(city,"\\("),
                          str_locate(city,'\\('),
                          0),
           city = city %>% 
             str_sub(0,index-1) %>% 
             str_remove_all("\\*")%>% 
             str_trim(),
           ## Remove [ ]
           index = ifelse(str_detect(city,"\\["),
                          str_locate(city,'\\['),
                          0),
           city = city %>% 
             str_sub(0,index-1) %>% 
             str_remove_all("\\*")%>% 
             str_trim(),
           ## Manually recode Aguascalientes
           city = city %>% 
             recode("Aguascalientes"="Aguascaliente")) %>% 
    select(-index)
}
