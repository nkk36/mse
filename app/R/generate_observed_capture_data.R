generate_observed_capture_data = function(data){
  
  d = histpos.t(3)
  Freq1 = length(intersect(intersect(data[["A"]],data[["B"]]),data[["C"]])) #In all
  Freq2 = length(intersect(data[["A"]],data[["B"]])) - Freq1 #In A and B, but not C
  Freq3 = length(intersect(data[["A"]],data[["C"]])) - Freq1 #In A and C, but not B
  Freq4 = length(data[["A"]]) - Freq1 - Freq2 - Freq3 #In A only
  Freq5 = length(intersect(data[["B"]],data[["C"]])) - Freq1 #In B and C, but not A
  Freq6 = length(data[["B"]]) - Freq1 - Freq2 - Freq5 #In B only
  Freq7 = length(data[["C"]]) - Freq1 - Freq3 - Freq5 #In C only
  
  #All frequences
  Freq = c(Freq1,
           Freq2,
           Freq3,
           Freq4,
           Freq5,
           Freq6,
           Freq7
  )
  d = cbind(d,Freq)
  d = as.data.frame(d)
  colnames(d) = c("A", "B", "C", "Freq")
  d
  
}