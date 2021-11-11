str_wrap_leaflet_legend_title = function(str){
  unique(str) %>% str_wrap(width  =20) %>% str_replace_all('\n','<br>')
}