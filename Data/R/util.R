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
             str_trim()) %>% 
    select(-index)
}

xwalk_colors =tibble(hex =  c("#F0F921","#F89441","#CC4678","#7E03A8","#0D0887")) %>% mutate(row = row_number())

categorize_jenks_edf_cold = function(dfTmp){
  
    ## Natural Breaks
    breaksTmp=  classIntervals(dfTmp$value,n=5, style = 'jenks')$brks
    dfCat = tibble(brk = breaksTmp) %>% 
      mutate(row = row_number(),
             brk =  case_when(
               row ==1 ~round(brk,2),
               TRUE ~  round(brk,2)+0.1),
             ahead = lead(brk, default = 9999999)-0.01,
             cat = case_when(
               row == 5 ~ glue("{brk} +"),
               TRUE ~ glue("{brk} - {ahead}"))) %>% 
      left_join(xwalk_colors)
    ## Assign Jenkins bins
    assigned_jenks = function(value,dfCat){
      dfCat %>%
        rowwise() %>%
        mutate(within = between(value, brk, ahead)) %>% 
        filter(within) %>% pull(cat) 
      
    }
    
    ## Return
    dfTmp %>%
      rowwise() %>% 
      mutate(cat = assigned_jenks(value,dfCat) ) %>% 
      ungroup() %>% 
      left_join(dfCat %>% select(cat, hex)) %>% 
      mutate( cat = factor(cat, levels =  dfCat$cat))
    
}

