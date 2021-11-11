UnivariateBeeswarm_UI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("swarm"), height = 600)
  )
}

UnivariateBeeswarm_Server <- function(id,dataFiltered ){
  moduleServer(id,function(input, output, session) {
    
    
    output$swarm <- renderPlotly({
      data = dataFiltered()
      if (is.null(data)) return()
      validate( need(nrow(data)>1, 'No Data Available, please select another attribute combination.'))
      

      ## Arrange by stratified varianble average
      xwalk_factor_order = data %>% 
        group_by(by) %>% 
        summarize(value = mean(value)) %>% 
        ungroup() %>% 
        arrange((value))
      
      dfTmpOrdered = data %>% mutate(by = factor(by, levels = xwalk_factor_order$by))
      
      ## Var Label
      var_short_tmp = unique(dfTmpOrdered$metric)
      
      ## 
      gg = dfTmpOrdered %>% 
        ggplot(mapping=aes(value,by, text = tooltip__beeswarmPlotly)) + 
        geom_quasirandom(aes(value,by,color=by),
                         groupOnX=FALSE)+
        theme_minimal()+
        labs(y = "",
             x= var_short_tmp)
      
      
      ggplotly(gg, tooltip = 'text' )
      
    })
    
    
    
    
  } )
}

