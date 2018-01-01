plot_model_estimates_range = function(data, total_population){
  
  d = data.frame(id = 1:8,x = round(data$results[,1]), y = 1:8, z = rep(total_population,8))
  
  ggplot(d, aes(x = x,y = y)) + 
    geom_point(aes(x, shape = "Population Estimate")) + 
    geom_line(aes(z, colour = "True Population")) + 
    labs(x = "Population", y = "Model", title = "Range of Model Estimates") +
    theme(legend.title=element_blank()) + 
    scale_x_continuous(label=comma)
  
}