###########################################################################################################################
#                                                                                                                         #
#                                             LOAD PACKAGES/FUNCTIONS/DATA                                                #
#                                                                                                                         #
###########################################################################################################################

# Load packages
library(shiny)
library(shinydashboard)

###########################################################################################################################
#                                                                                                                         #
#                                                    USER INTERFACE                                                       #
#                                                                                                                         #
###########################################################################################################################


#################################################################
#                                                               #
#                         HEADER                                #
#                                                               #
#################################################################

Header = dashboardHeader(title = "Multiple Systems Estimation", 
                         titleWidth = 300)


#################################################################
#                                                               #
#                       SIDEBAR                                 #
#                                                               #
#################################################################

Sidebar = dashboardSidebar(
  
  numericInput(inputId= "total_population", 
               label = "Total Population:", 
               value = 10000, 
               min = 1, 
               max = 1000000000),
  numericInput(inputId = "list1_size", 
               label = "List 1 Size:", 
               value = 1000, 
               min = 1, 
               max = 1000000000),
  numericInput(inputId = "list2_size", 
               label = "List 2 Size:", 
               value = 1000, 
               min = 1, 
               max = 1000000000),
  numericInput(inputId = "list3_size", 
               label = "List 3 Size:", 
               value = 1000, 
               min = 1, 
               max = 1000000000),
  checkboxInput(inputId = "list_dependence_binary", 
                label = "List Dependence", 
                value = FALSE),
  checkboxInput(inputId = "unequal_capture_prob_binary", 
                label = "Unequal Capture Probability", 
                value = FALSE)
) # End sidebar


#################################################################
#                                                               #
#                           BODY                                #
#                                                               #
#################################################################

Body = dashboardBody(
  
  tags$head(includeScript("google-analytics.js")),
  
  fluidRow(
    
    valueBoxOutput(outputId = "total_population", 
                   width = 2),
    valueBoxOutput(outputId = "total_population_estimate", 
                   width = 4),
    valueBoxOutput(outputId = "total_population__estimate_error", 
                   width = 2),
    valueBoxOutput(outputId = "percent_population_known", 
                   width = 2),
    valueBoxOutput(outputId = "list_overlap_percentage", 
                   width = 2)
    
  ),
  
  fluidRow(
    box(
      plotOutput(outputId = "venn_diagram")
    ),
    box(
      plotOutput(outputId = "pdf_best_model")
    )
  ),
  
  fluidRow(
    box(
      width = 6,
      plotOutput(outputId = "model_estimates_range")
    ),
    box(
      width = 6,
      dataTableOutput(outputId = "all_model_estimates_table")
    )
  )
  
) # End Body


#################################################################
#                                                               #
#                       DEFINE UI                               #
#                                                               #
#################################################################

ui = dashboardPage(
  Header,
  Sidebar,
  Body
)