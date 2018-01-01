library(shiny)
library(shinydashboard)
library(scales)
library(Rcapture)
library(VennDiagram)
library(ggplot2)
library(formattable)
library(dplyr)


function(input, output, session) {
  
  Sample.Data = reactive({
    
    Objects = 1:input$TotalPop
    ProbWeights = base::sample(x = 1:10, size = input$TotalPop,replace = TRUE)
    
    if (input$UneqCapProb == TRUE){
      
      Sample.A = base::sample(Objects, size = input$LS1, prob = ProbWeights)
      
      if (input$DepLists == TRUE){
        
        NotA = setdiff(Objects, Sample.A)
        ListSize = round(0.25*input$LS2)
        Sample.NotA = base::sample(NotA, size = ListSize)
        A.NotA = c(Sample.A,Sample.NotA)
        Sample.B = base::sample(A.NotA, size = input$LS2)
        
      }
      else{
        Sample.B = base::sample(Objects, size = input$LS2, prob = ProbWeights)
      }
      
      Sample.C = base::sample(Objects, size = input$LS3, prob = ProbWeights)
    }
    else{
      
      Sample.A = base::sample(Objects, size = input$LS1)
      
      if (input$DepLists == TRUE){
        
        NotA = setdiff(Objects,Sample.A)
        ListSize = round(0.25*input$LS2)
        Sample.NotA = base::sample(NotA, size = ListSize)
        A.NotA = c(Sample.A,Sample.NotA)
        Sample.B = base::sample(A.NotA, size = input$LS2)
        
      }
      else{
        Sample.B = base::sample(Objects, size = input$LS2)
      }
      
      Sample.C = base::sample(Objects, size = input$LS3)
      
    }
    
    d = list(A = Sample.A, B = Sample.B, C = Sample.C)
    
    d
    
  })
  
  ObsCaptData = reactive({
    
    d = histpos.t(3)
    Freq1 = length(intersect(intersect(Sample.Data()[["A"]],Sample.Data()[["B"]]),Sample.Data()[["C"]])) #In all
    Freq2 = length(intersect(Sample.Data()[["A"]],Sample.Data()[["B"]])) - Freq1 #In A and B, but not C
    Freq3 = length(intersect(Sample.Data()[["A"]],Sample.Data()[["C"]])) - Freq1 #In A and C, but not B
    Freq4 = length(Sample.Data()[["A"]]) - Freq1 - Freq2 - Freq3 #In A only
    Freq5 = length(intersect(Sample.Data()[["B"]],Sample.Data()[["C"]])) - Freq1 #In B and C, but not A
    Freq6 = length(Sample.Data()[["B"]]) - Freq1 - Freq2 - Freq5 #In B only
    Freq7 = length(Sample.Data()[["C"]]) - Freq1 - Freq3 - Freq5 #In C only
    
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
  })
  
  results = reactive({
    
    closedpMS.t(ObsCaptData(),dfreq = TRUE)
    
  })
  
  
  output$VennDiagram = renderPlot({
    draw.triple.venn(area1 = sum(ObsCaptData()$Freq[c(1,2,3,4)]), 
                     area2 = sum(ObsCaptData()$Freq[c(1,2,5,6)]),
                     area3 = sum(ObsCaptData()$Freq[c(1,3,5,7)]),
                     n12 = sum(ObsCaptData()$Freq[c(1,2)]),
                     n23 = sum(ObsCaptData()$Freq[c(1,5)]),
                     n13 = sum(ObsCaptData()$Freq[c(1,3)]),
                     n123 = ObsCaptData()$Freq[1],
                     category = c("List 1", "List 2", "List 3"),
                     fill = c("skyblue", "pink1", "mediumorchid"),
                     lty = "blank",
                     euler.d = TRUE,
                     scaled = TRUE
    )
  })
  
  output$BarModel = renderPlot({
    
    index.best.est = which.min(abs(results()$results[,1] - input$TotalPop))
    
    ggplot(data = data.frame(x = c(0, 2*results()$results[index.best.est,1])), aes(x)) +
      stat_function(fun = dnorm, n = 101, args = list(mean = results()$results[index.best.est,1], sd = results()$results[index.best.est,2])) + 
      labs(x = "Population Estimate", y = "Probability", title = "Probability Distribution of Most Accurate Model") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(label=comma)
    
    
  })
  
  output$EstRange = renderPlot({
    
    d = data.frame(id = 1:8,x = round(results()$results[,1]), y = 1:8, z = rep(input$TotalPop,8))
    
    ggplot(d, aes(x = x,y = y)) + 
      geom_point(aes(x, shape = "Population Estimate")) + 
      geom_line(aes(z, colour = "True Population")) + 
      labs(x = "Population", y = "Model", title = "Range of Model Estimates") +
      theme(legend.title=element_blank()) + 
      scale_x_continuous(label=comma)
    
    
  })
  
  output$TotalPop <- renderValueBox({
    
    valueBox(
      comma(input$TotalPop), 
      "True Total Population", 
      icon = icon("list"),
      color = "aqua"
    )
  })
  
  output$TotalPopEst <- renderValueBox({
    
    index.best.est = which.min(abs(results()$results[,1] - input$TotalPop))
    valueBox(
      comma(round(results()$results[index.best.est,1])), 
      "Estimated Total Population", 
      icon = icon("list"),
      color = "aqua"
    )
    
  })
  
  output$TotalPopError <- renderValueBox({
    
    d = round(min(abs(results()$results[,1] - input$TotalPop)))
    valueBox(
      comma(d), "Error", 
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  output$PercPop <- renderValueBox({
    
    valueBox(
      paste0(formatC(sum(ObsCaptData()$Freq)/input$TotalPop*100,1,format = "f"),"%"), 
      "Known Percentage of Population", 
      icon = icon("pie-chart"),
      color = "olive"
    )
  })
  
  output$ListPercOverlap = renderValueBox({
    
    valueBox(
      paste0(formatC(length(intersect(Sample.Data()[["A"]],Sample.Data()[["B"]]))/length(Sample.Data()[["B"]])*100,1,format = "f"), "%"), 
      "Lists 1 and 2: Percentage Overlap", 
      icon = icon("pie-chart"), 
      color = "olive"
    )
    
  })
  
  output$dtable = renderDataTable({
    
    d = as.data.frame(round(results()$results[,1:2]))
    d = comma(arrange(d,desc(abundance)))
    d = cbind(c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8"), d)
    colnames(d) = c("Model","EstimatedPopulation", "Standard Deviation")
    d
    
  })
  
  
}
