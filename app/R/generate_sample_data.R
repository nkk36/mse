generate_sample_data = function(total_population, 
                                list1_size, list2_size, list3_size, list_dependence_binary, unequal_capture_prob_binary){
  
  objects = 1:total_population
  probability_weights = base::sample(x = 1:10, size = total_population,replace = TRUE)
  
  if (unequal_capture_prob_binary == TRUE){
    
    sample_A = base::sample(objects, size = list1_size, prob = probability_weights)
    
    if (list_dependence_binary == TRUE){
      
      not_A = setdiff(objects, sample_A)
      list_size = round(0.25*list2_size)
      sample_not_A = base::sample(not_A, size = list_size)
      A_and_not_A = c(sample_A,sample_not_A)
      sample_B = base::sample(A_and_not_A, size = list2_size)
      
    }
    else{
      sample_B = base::sample(objects, size = list2_size, prob = probability_weights)
    }
    
    sample_C = base::sample(objects, size = list3_size, prob = probability_weights)
  }
  else{
    
    sample_A = base::sample(objects, size = list1_size)
    
    if (list_dependence_binary == TRUE){
      
      not_A = setdiff(objects,sample_A)
      list_size = round(0.25*list2_size)
      sample_not_A = base::sample(not_A, size = list_size)
      A_and_not_A = c(sample_A,sample_not_A)
      sample_B = base::sample(A_and_not_A, size = list2_size)
      
    }
    else{
      sample_B = base::sample(objects, size = list2_size)
    }
    
    sample_C = base::sample(objects, size = list3_size)
    
  }
  
  d = list(A = sample_A, B = sample_B, C = sample_C)
  
  d
  
}