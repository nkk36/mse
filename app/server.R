###########################################################################################################################
#                                                                                                                         #
#                                             LOAD PACKAGES/FUNCTIONS/DATA                                                #
#                                                                                                                         #
###########################################################################################################################

# Load packages
library(shiny)
library(shinydashboard)
library(scales)
library(Rcapture)
library(VennDiagram)
library(ggplot2)
library(formattable)
library(dplyr)

# Load functions
source("R/generate_sample_data.R")
source("R/generate_observed_capture_data.R")
source("R/plot_pdf_best_model.R")
source("R/plot_model_estimates_range.R")
source("R/get_all_model_estimates_table.R")



###########################################################################################################################
#                                                                                                                         #
#                                                         SERVER                                                          #
#                                                                                                                         #
###########################################################################################################################

function(input, output, session) {
  
  #################################################################
  #                                                               #
  #                       REACTIVE DATA                           #
  #                                                               #
  #################################################################
  
  sample_data = reactive({
    
    generate_sample_data(total_population = input$total_population, 
                         list1_size = input$list1_size, 
                         list2_size = input$list2_size, 
                         list3_size = input$list3_size, 
                         list_dependence_binary = input$list_dependence_binary, 
                         unequal_capture_prob_binary = input$unequal_capture_prob_binary)
    
  })
  
  observed_capture_data = reactive({
    
    generate_observed_capture_data(data = sample_data())
    
  })
  
  all_model_results = reactive({
    
    closedpMS.t(observed_capture_data(),dfreq = TRUE)
    
  })
  
  
  #################################################################
  #                                                               #
  #                         OUTPUTS                               #
  #                                                               #
  #################################################################
  
  output$venn_diagram = renderPlot({
    draw.triple.venn(area1 = sum(observed_capture_data()$Freq[c(1,2,3,4)]), 
                     area2 = sum(observed_capture_data()$Freq[c(1,2,5,6)]),
                     area3 = sum(observed_capture_data()$Freq[c(1,3,5,7)]),
                     n12 = sum(observed_capture_data()$Freq[c(1,2)]),
                     n23 = sum(observed_capture_data()$Freq[c(1,5)]),
                     n13 = sum(observed_capture_data()$Freq[c(1,3)]),
                     n123 = observed_capture_data()$Freq[1],
                     category = c("List 1", "List 2", "List 3"),
                     fill = c("skyblue", "pink1", "mediumorchid"),
                     lty = "blank",
                     euler.d = TRUE,
                     scaled = TRUE
    )
  })
  
  output$pdf_best_model = renderPlot({
    
    plot_pdf_best_model(data = all_model_results(),
                        total_population = input$total_population)
    
  })
  
  output$model_estimates_range = renderPlot({

    plot_model_estimates_range(data = all_model_results(),
                               total_population = input$total_population)
    
  })
  
  output$total_population <- renderValueBox({
    
    valueBox(
      comma(input$total_population), 
      "True Total Population", 
      icon = icon("list"),
      color = "aqua"
    )
  })
  
  output$total_population_estimate <- renderValueBox({
    
    index.best.est = which.min(abs(all_model_results()$results[,1] - input$total_population))
    valueBox(
      comma(round(all_model_results()$results[index.best.est,1])), 
      "Estimated Total Population", 
      icon = icon("list"),
      color = "aqua"
    )
    
  })
  
  output$total_population__estimate_error <- renderValueBox({
    
    d = round(min(abs(all_model_results()$results[,1] - input$total_population)))
    valueBox(
      comma(d), "Error", 
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  output$percent_population_known <- renderValueBox({
    
    valueBox(
      paste0(formatC(sum(observed_capture_data()$Freq)/input$total_population*100,1,format = "f"),"%"), 
      "Known Percentage of Population", 
      icon = icon("pie-chart"),
      color = "olive"
    )
  })
  
  output$list_overlap_percentage = renderValueBox({
    
    valueBox(
      paste0(formatC(length(intersect(sample_data()[["A"]],sample_data()[["B"]]))/length(sample_data()[["B"]])*100,1,format = "f"), "%"), 
      "Lists 1 and 2: Percentage Overlap", 
      icon = icon("pie-chart"), 
      color = "olive"
    )
    
  })
  
  output$all_model_estimates_table = renderDataTable({
    
    get_all_model_estimates_table(data = all_model_results())
    
  })
  
  
}
