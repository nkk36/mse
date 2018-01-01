get_all_model_estimates_table = function(data){
  
  d = as.data.frame(round(data$results[,1:2]))
  d = scales::comma(arrange(d,desc(abundance)))
  d = cbind(c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8"), d)
  colnames(d) = c("Model","EstimatedPopulation", "Standard Deviation")
  d
  
}