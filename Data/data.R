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
  
  ## Load crosswalks
  load("../../SALURBAL Dashboard Portal/Data/Clean/Crosswalks/xwalk_iso2.rdata")
  load("Clean/xwalk_l1.rdata")
  load("Clean/xwalk_l1_server.rdata")
  
}


{  # 1. Format Imported data for dashboard ------
  ## Process Jeff data  
  cleaned_data_jeff_all_variables =  df_import_65plus %>% 
    mutate(grp = "65+") %>% 
    bind_rows(df_import_allAges %>% mutate(grp = "Crude")) %>% 
    mutate_all(~as.character(.x)) %>% 
    rename(climate = level_1_climate,
           age = grp) %>% 
    pivot_longer(-c(city, salid1, country,age, climate), names_to = 'metric') %>% 
    arrange(metric, country,age,climate, salid1) 
  
  cleaned_data_jeff = cleaned_data_jeff_all_variables%>% 
    filter(metric%in%c('average_temperature','median_temperature',
                       'af_due_to_cold','af_due_to_extreme_cold',
                       'af_due_to_heat','af_due_to_extreme_heat')) %>% 
    mutate( value = ifelse(str_detect(metric,'af'),
                           as.numeric(value) %>% round(2),
                           as.numeric(value) %>% round(1))) 
  ### process Josiah Data
  cleaned_data_josiah = df_JK %>% 
    mutate_all(~as.character(.x)) %>% 
    pivot_longer(cols = c("rr_99vs95_per1c",  "rrcat_p99"  ,   "rr_1vs5_per1c" ,  "rrcat_p1")) %>% 
    mutate(type = ifelse(str_detect(name, 'per1c'),"value",'cat'),
           metric = ifelse(str_detect(name, '99'),"RR of heat-related mortality","RR of cold-related mortality")) %>% 
    select(-name) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    mutate(value = as.numeric(value) %>% round(3),
           cat = factor(cat, levels =   c("<= 1.000" ,"1.001 - 1.049", "1.050 - 1.099", "1.100 - 1.149", "1.150 +"  ))) %>% 
    left_join(cleaned_data_jeff %>% select(salid1, city, country, climate) %>% distinct())
}


{# 2.  Server data  ------
  
  { # 2.1 cleaned__tidy_data ------
    #'  object for maps and plots
    cleaned__tidy_data = cleaned_data_jeff %>% 
      bind_rows(cleaned_data_josiah) %>% 
      left_join(df__l1_centroids %>% select(salid1, long,lat), by = 'salid1') %>% 
      ## Add iso2
      left_join(xwalk_l1_server %>% select(salid1, country2 = country)) %>% 
      left_join(xwalk_iso2 %>% select(country2 = country, iso2)) %>% 
      mutate(
        ## Recode age groups
        age = age %>% recode("Crude"="All-Ages"),
        ## Recode metric names
        metric = metric %>% recode(
          "average_temperature"="Mean Temperature",
          'median_temperature'='Median Temperature',
          "af_due_to_cold"="EDF due to cold",
          "af_due_to_extreme_cold"="EDF due to extreme cold",
          "af_due_to_heat"='EDF due to heat',
          'af_due_to_extreme_heat'= 'EDF due to extreme heat',
          "RR of cold-related mortality"="Mortality risk per 1C lower extreme cold",
          "RR of heat-related mortality"="Mortality risk per 1C higher extreme heat"),
        ## Create tooltips
        tooltip__map = glue(
          '<span class="map-tooltip-header">{city}, {iso2}</span><br />
        {metric}: {value}'),
        tooltip__beeswarmPlotly = glue(
          '<b>{city}, {iso2}</b>
        Country: {country}
        Value: {value}'),
        ## Add Colors for catgorical data
        hex = cat %>% recode(
          "<= 1.000"="#F0F921",
          "1.001 - 1.049"="#F89441",
          "1.050 - 1.099"="#CC4678",
          "1.100 - 1.149"="#7E03A8",
          "1.150 +"="#0D0887")
      ) 
    
  }
  { # 2.2 cleaned__tidy_metadata ------
    #' Metadata for city specific details
    cleaned__tidy_metadata = cleaned_data_jeff_all_variables %>% 
      filter(age == "Crude") %>% 
      select(salid1, city, metric, value) %>% 
      distinct() %>% 
      ### Filter out metrics to display
      filter(str_detect(metric,"temperature")) %>%
      ## Rename  
      rowwise() %>% 
      mutate(index = str_locate(metric,"_")[1],
             metric = str_sub(metric,1,index-1) %>% str_trim() %>% str_to_title()) %>% 
      ungroup() %>% 
      ## Reoder
      mutate(order = metric %>% recode("Average"=1,
                                       "P1"=2,
                                       "P5"=3,
                                       "Median"=4,
                                       "P95"=5,
                                       "P99"=6)) %>% 
      arrange(salid1, order) %>% 
      select(-index,-order) %>% 
      ## Format Temperature values
      mutate(value = as.numeric(value) %>% round(1)) 
  }
  
  { # 2.3 Save  ------
    save(cleaned__tidy_data,cleaned__tidy_metadata,
         file = "../App/R/Data/cleaned__data_server.rdata")
  }
  
}

{# 3. UI data------
  
  { # 3.1 Input options ------
    #' choices for inputs 
    options__input = list()
    xwalk_metrics = cleaned__tidy_data %>% count(metric) %>% 
      mutate(
        grp = metric %>% recode(
          'Mortality risk per 1C higher extreme heat'="Relative Risk",
          'Mortality risk per 1C lower extreme cold'="Relative Risk",
          'Mean Temperature'="Temperature",
          'Median Temperature'="Temperature",
          'EDF due to heat'="Excess Death Fraction (Hot)",
          'EDF due to extreme heat'="Excess Death Fraction (Hot)",
          'EDF due to cold'="Excess Death Fraction (Cold)",
          'EDF due to extreme cold'="Excess Death Fraction (Cold)"
        ),
        order_grp = grp %>% recode(
          "Relative Risk"=1,
          "Temperature"=4,
          "Excess Death Fraction (Hot)"=2,
          "Excess Death Fraction (Cold)"=3
        ),
        order_metric = metric %>% recode(
          'Mortality risk per 1C higher extreme heat'=1,
          'Mortality risk per 1C lower extreme cold'=2,
          'Mean Temperature'=1,
          'Median Temperature'=2,
          'EDF due to heat'=2,
          'EDF due to extreme heat'=1,
          'EDF due to cold'=2,
          'EDF due to extreme cold'=1
        )
      ) %>% 
      arrange(order_grp, order_metric)
    xwalk_metrics_bivar1 = xwalk_metrics %>% filter(grp=='Relative Risk')
    xwalk_metrics_bivar2_hot = xwalk_metrics %>% filter(grp%in%c('Excess Death Fraction (Hot)',
                                                                 'Temperature'))
    xwalk_metrics_bivar2_cold = xwalk_metrics %>% filter(grp%in%c('Excess Death Fraction (Cold)',
                                                                  'Temperature'))
    
    
    ## Univariate Choices
    options__input$metric = map(unique(xwalk_metrics$grp), ~{xwalk_metrics %>%  filter(grp==.x) %>% pull(metric)  }) %>% 
      set_names( unique(xwalk_metrics$grp) )
    options__input$age = unique(cleaned__tidy_data$age) %>% sort(decreasing = T)
    options__input$by = c("Country"="country",'Climate'='climate')
    
    ## Bivariate choices
    options__input$bivar_metric1 =  map(unique(xwalk_metrics_bivar1$grp), ~{xwalk_metrics_bivar1 %>%  filter(grp==.x) %>% pull(metric)  }) %>% 
      set_names( unique(xwalk_metrics_bivar1$grp) )
    options__input$bivar_metric2_hot =  map(unique(xwalk_metrics_bivar2_hot$grp), ~{xwalk_metrics_bivar2_hot %>%  filter(grp==.x) %>% pull(metric)  }) %>% 
      set_names( unique(xwalk_metrics_bivar2_hot$grp) )
    options__input$bivar_metric2_cold =  map(unique(xwalk_metrics_bivar2_cold$grp), ~{xwalk_metrics_bivar2_cold %>%  filter(grp==.x) %>% pull(metric)  }) %>% 
      set_names( unique(xwalk_metrics_bivar2_cold$grp) )
    
    ## City Choices
    citiesTmp = cleaned__tidy_data %>% select(salid1, city) %>%  distinct()
    options__input$cities = citiesTmp$salid1 %>% set_names(citiesTmp$city)
    
    ## color/hex choices
    options__input$leaflet_legend_labels = cleaned__tidy_data %>% drop_na() %>%  count(cat, hex) %>% pull(cat)
    options__input$leaflet_legend_colors = cleaned__tidy_data %>% drop_na()  %>% count(cat, hex) %>% pull(hex)
    
  }
  
  
  { # 2.3 Save  ------
    save(options__input,
         file = "../App/R/Data/cleaned__data_ui.rdata")
  }
  
}

