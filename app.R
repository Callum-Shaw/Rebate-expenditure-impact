#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(shinyWidgets)

source('function.R')


grattan_slider <- function(variable, title, subtitle, default = 0){
    
    sliderInput(inputId = variable, 
                
                label = HTML(paste0(tags$span(style = "color:#6A737B", title),
                                    
                                    tags$br(),
                                    
                                    tags$span(style = "font-weight:normal; color:#6A737B; font-size:12px", subtitle))),
                
                min = 0, max = 45,
                
                value = default, step = 1)
    
}


grattan_check <- function(variable, title, value){
    
    checkboxInput(inputId = variable, 
                label = HTML(paste0(tags$span(style = "font-weight:normal;color:#6A737B", title))),
                value = value)
    
}

ui <- fluidPage(
    chooseSliderSkin("Flat", color = "#F68B33"),
    # Application title
    title = "Rebate expenditure",
    # Sidebar with a slider input for number of bins 
    hr(),
    
    titlePanel(
                    span("What is the impact of changes in the PHI rebate on private contributions?",
                         
                         style = "color:#6A737B; font-weight:bold; font-size:24px")),
    hr(),
    
    sidebarLayout(
        sidebarPanel(grattan_slider(var = "q1",
                       
                       title = "What proportion of the population will have private health insurance without the private health insurance rebate?",
                       
                       subtitle = "% of population")
        ,
        grattan_check("q2",
                    "Include Government expenditure on the rebate on general insurance (extras) in total rebate expenditure, in addition to expenditure on the private hospital rebate",
                    value = FALSE),
        grattan_check("q3",
                      "Include extras expenditure in the revenue generated",
                      value = FALSE),
        grattan_check("q4",
                      "Include out-of-pocket costs associated with public and private hospital care in the contributions generated from the rebate",
                      value = FALSE))
    ,mainPanel(textOutput("txtOutput")))

)

server <- function(input, output) {
    output$txtOutput = renderText({
        paste0("Every dollar of rebate expenditure generates an extra $",round(estimated_impact(input$q1,input$q2,input$q3,input$q4),2)," of private contributions for healthcare.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
