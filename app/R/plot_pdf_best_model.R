plot_pdf_best_model = function(data, total_population){
  
  index.best.est = which.min(abs(data$results[,1] - total_population))
  
  ggplot(data = data.frame(x = c(0, 2*data$results[index.best.est,1])), aes(x)) +
    stat_function(fun = dnorm, n = 101, args = list(mean = data$results[index.best.est,1], sd = data$results[index.best.est,2])) + 
    labs(x = "Population Estimate", y = "Probability", title = "Probability Distribution of Most Accurate Model") +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(label=comma)
  
}