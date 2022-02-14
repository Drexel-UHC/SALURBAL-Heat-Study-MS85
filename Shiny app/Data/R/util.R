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

print_digits = function(x){
  if((x-floor(x))==0){paste0(x,".0")}else {x}
}

categorize_jenks_edf_cold = function(dfTmp){
  
    ## Natural Breaks
    breaksTmp=  classIntervals(dfTmp$value,n=5, style = 'jenks')$brks
    dfCat = tibble(brk = breaksTmp) %>% 
      mutate(row = row_number(),
             brk =  case_when(
               row ==1 ~round(brk,2),
               TRUE ~  round(brk,2)+0.01),
             ahead = lead(brk, default = 9999999)-0.01
            ) %>% 

      left_join(xwalk_colors) %>% 
      rowwise() %>% 
      mutate( cat = case_when(
        row == 5 ~ glue("{print_digits(round(brk,1))} +"),
        TRUE ~ glue("{print_digits(round(brk,1))} - {print_digits(round(ahead+0.01,1)-0.1)}"))) %>% 
      ungroup()
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

