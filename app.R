library(shiny)
library(shinydashboard)
library(bupaR)
library(edeaR)
library(eventdataR)
library(processmapR)
library(processmonitR)
library(xesreadR)
library(petrinetR)
library(processanimateR)
ui <- dashboardPage(
    dashboardHeader(title = "PM Dashboard", disable = F),
    dashboardSidebar(
        sidebarMenu(
        selectInput("eventlog", h3("Select Event Log"),
                    choices = c("Hospital Billing","Traffic Fines", "Sepsis")),
        sliderInput("slider1", h3("Trace Frequency"),
                    min = 60, max = 100, value = 100),
        sliderInput("slider2",h3("Activity Frequency"),
                    min = 60, max = 100, value = 100),
        menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
        menuItem("FAQ",tabName = "FAQ",icon = icon("question")),
        menuItem("Github Repository", tabName = "repo", icon=icon("github"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
            fluidRow(
                uiOutput("ui")
            )
            ),
            tabItem(tabName = "repo",
                    a("Process Mining Dashboard in Github",href="https://www.github.com/FedeScenna/ProcessMiningDashboard")),
            tabItem(tabName = "FAQ",
                    h2("Frequently Asked Questions",
                       h4("What is this project about?"),
                       p("This is a dashboard using only event log data as a 
                         sample of the results that can be archieved by applying process mining techniques.",
                        h4("...and what is process mining?"),
                           p("Process mining is a set of tools and techniques that fill the gap where standard data science is missing. 
                             It is about applying data mining to event log type data.",
                        h4("And what can be archieved with process mining?"),
                            p("There are three main types of process mining that ilustrate what can be done. The first type is discovery. 
                              It is about taking an event log and producing a model explaining the recorded behaviour.
                              The second type is conformance. In this case, a process model is compared to an event log in order to check if the process model conforms to the 
                              event log in reality and vice versa. Lastly, the third type is enhacement. While conformance aims to check the process vs reality, enhancement aims to change or extend the a-priori model.", 
                        h4("Will I find all types of process mining applied in this dashboard?"),
                            p("No. In this dashboard you will only find discovery process mining.",
                        h4("Who created this dashboard?"),
                            p("This dashboard was created by Federico Scenna. You can access the github repository in the Github repo section of this dashboard.")))))
                       )
                    )
            )
        )
    )
server <- function(input, output) {
    #return requested dataset
    selectedEventlog <- reactive({
        switch (input$eventlog, 
                "Sepsis" = sepsis, 
                "Hospital Billing" = hospital_billing,
                "Traffic Fines" = traffic_fines)
    })
    
    output$ui <- renderUI({
        t <- input$slider1
        a <- input$slider2
        el <- selectedEventlog() %>%
            filter_activity_frequency(percentage = a/100) %>%
            filter_trace_frequency(percentage = t/100, reverse=F)
        withProgress(
        tagList(el %>% process_map(),
        message="Calculating..."))
    })
}
shinyApp(ui, server)