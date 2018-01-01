library(shiny)
library(shinydashboard)

Header = dashboardHeader(title = "Multiple Systems Estimation", titleWidth = 300)

Sidebar = dashboardSidebar(
  
  numericInput("TotalPop", "Total Population:", 10000, min = 1, max = 1000000000),
  numericInput("LS1", "List 1 Size:", 1000, min = 1, max = 1000000000),
  numericInput("LS2", "List 2 Size:", 1000, min = 1, max = 1000000000),
  numericInput("LS3", "List 3 Size:", 1000, min = 1, max = 1000000000),
  checkboxInput("DepLists", "List Dependence", value = FALSE),
  checkboxInput("UneqCapProb", "Unequal Capture Probability", value = FALSE)
  
  
)

Body = dashboardBody(
  
  fluidRow(
    
    valueBoxOutput("TotalPop", width = 2),
    valueBoxOutput("TotalPopEst", width = 4),
    valueBoxOutput("TotalPopError", width = 2),
    valueBoxOutput("PercPop", width = 2),
    valueBoxOutput("ListPercOverlap", width = 2)
    
  ),
  
  fluidRow(
    box(
      plotOutput("VennDiagram")
    ),
    box(
      plotOutput("BarModel")
    )
  ),
  
  fluidRow(
    box(
      width = 6,
      plotOutput("EstRange")
    ),
    box(
      width = 6,
      dataTableOutput("dtable")
    )
  )
  
)

ui = dashboardPage(
  Header,
  Sidebar,
  Body
)