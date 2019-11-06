library(shiny)
library(shinydashboard)
library(bupaR)
library(edeaR)
library(eventdataR)
library(processmapR)
library(processmonitR)
library(xesreadR)
library(petrinetR)

loadData <- function(el){
    read_xes(paste("data/",as.character(el), ".xes", sep=""))
}

createEL<- function(df){
    df %>%
    eventlog(case_id="CASE_concept_name", 
             activity_id="activity_id",
             activity_instance_id = "activity_instance_id",
             timestamp = "timestamp",
             lifecycle_id = "lifecycle_id",
             resource_id = "resource_id") %>%
        return()
}

repairExample <- loadData("repairExample") %>%
    createEL()
reviewing <- loadData("reviewing") %>%
    createEL()


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        selectInput("eventlog", h3("Select Event Log"),
                    choices = list("Reviewing" = "reviewing",
                                   "Repair Example" = "repairExample"), selected = "repairExample"),
        sliderInput("slider1", h3("Trace Frequency"),
                    min = 60, max = 100, value = 60)
    ),
    dashboardBody(
        uiOutput("ui")
    ))
server <- function(input, output) { 
    output$ui <- renderUI({
        r <- input$slider1
        tagList(filter_trace_frequency(input$eventlog,
                                       percentage = r/100, reverse = F) %>% process_map())
    })
}
shinyApp(ui, server)