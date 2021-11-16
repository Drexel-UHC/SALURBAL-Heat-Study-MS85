UnivariateBeeswarm_UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("swarm"), height = 600)
  )
}

UnivariateBeeswarm_Server <- function(id,dataFiltered,by ){
  moduleServer(id,function(input, output, session) {
    
    
    output$swarm <- renderPlotly({
      dataFiltered = dataFiltered() %>% 
        mutate_(by={{by}}) 
      
      
      if (is.null(dataFiltered)) return()
      validate( need(nrow(dataFiltered)>1, 'No Data Available, please select another attribute combination.'))
      

      ## Arrange by stratified varianble average
      xwalk_factor_order = dataFiltered %>% 
        group_by(by) %>% 
        summarize(value = mean(value)) %>% 
        ungroup() %>% 
        arrange((value))
      
      dfTmpOrdered = dataFiltered %>% mutate(by = factor(by, levels = xwalk_factor_order$by))
      
      ## Var Label
      var_short_tmp = unique(dfTmpOrdered$metric)
      
      ## Plot
      gg = dfTmpOrdered %>% 
        ggplot(mapping=aes(value,by, text = tooltip__beeswarmPlotly)) + 
        geom_quasirandom(aes(value,by,color=by),
                         groupOnX=FALSE)+
        theme_minimal()+
        labs(y = "",
             x= var_short_tmp)+ 
        scale_color_discrete(name = str_to_title(by))
      
      
      ggplotly(gg, tooltip = 'text' )
      
    })
    
    
    
    
  } )
}

