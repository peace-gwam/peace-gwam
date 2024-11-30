# __________________
# dashboard.R
# created by Peace Gwam on 18Nov24
# creates dashboard of figures and charts
# see appendix for more details
# __________________


# libraries
library(shiny)
library(tidyverse)
library(shinydashboard)
library(janitor)

# Define the UI
ui <- fluidPage(
  titlePanel("Self-Service Feedback Survey Results"),
  tabsetPanel(
    tabPanel("About Respondents",
             tableOutput("broker_table")),
    tabPanel("Usability Score Distribution", 
             # Dropdown for plot selection
             selectInput("uxum_type", "Select Respondent Type:",
                         choices = list("All Observations" = "uxum_hist",
                                        "Brokers" = "uxum_hist_brokers",
                                        "Account Managers" = "uxum_hist_acct",
                                        "General Agents" = "uxum_hist_ga")),
             plotOutput("uxum_hist_dynamic"),
             tableOutput("uxum_table_dynamic"),
             textOutput("usability_mean")
    ),
    tabPanel("CSAT Score Distritribution",
             selectInput("csat_type", "Select Respondent Type:",
                         choices = list("All Observations" = "csat_hist",
                                        "Brokers" = "csat_hist_broker",
                                        "Account Managers" = "csat_hist_acct",
                                        "General Agents" = "csat_hist_ga")),
             plotOutput("csat_hist_dynamic"),
             tableOutput("csat_table_dynamic"),
             textOutput("csat_mean")
      
    )
  ))

server <- function(input, output) {
  # Render dynamic histogram plot
  output$uxum_hist_dynamic <- renderPlot({
    plot_obj <- switch(input$uxum_type,
                       "uxum_hist" = uxum_hist, 
                       "uxum_hist_brokers" = uxum_hist_brokers,
                       "uxum_hist_acct" = uxum_hist_acct,
                       "uxum_hist_ga" = uxum_hist_ga)
    print(plot_obj) 
  })
  
  # Render broker table
  output$broker_table <- renderTable({
    broker_table 
  })
  output$uxum_table_dynamic <- renderTable({
    switch(input$uxum_type,
           "uxum_hist" = uxum_table,  
           "uxum_hist_brokers" = uxum_table_brokers,
           "uxum_hist_acct" = uxum_table_acct,
           "uxum_hist_ga" = uxum_table_ga)
  })
  
  # usability mean dynamically based on input selection
  output$usability_mean <- renderText({
    switch(input$uxum_type,
           "uxum_hist" = paste("Total mean of usability score:", round(mean(uxum$final_score, na.rm = TRUE), 1)),
           "uxum_hist_brokers" = paste("Total mean of usability score for brokers:", round(mean(uxum_broker$final_score, na.rm = TRUE), 1)),
           "uxum_hist_acct" = paste("Total mean of usability score for account managers:", round(mean(uxum_acct$final_score, na.rm = TRUE), 1)),
           "uxum_hist_ga" = paste("Total mean of usability score for GAs:", round(mean(uxum_ga$final_score, na.rm = TRUE), 1))
    )
  })
  # Render dynamic histogram plot
  output$csat_hist_dynamic <- renderPlot({
    plot_obj <- switch(input$csat_type,
                       "csat_hist" = csat_hist, 
                       "csat_hist_broker" = csat_hist_broker,
                       "csat_hist_acct" = csat_hist_acct,
                       "csat_hist_ga" = csat_hist_ga)
    print(plot_obj) 
}) 
  output$csat_table_dynamic <- renderTable({
    switch(input$csat_type,
           "csat_hist" = csat_table,  
           "csat_hist_broker" = csat_table_broker,
           "csat_hist_acct" = csat_table_acct,
           "csat_hist_ga" = csat_table_ga)
  })
  output$csat_mean <- renderText({
    switch(input$csat_type,
           "csat_hist" = paste("Total CSAT Score:", round(total_csat_score * 100, 1),"%"),
           "csat_hist_broker" = paste("Total CSAT Score for brokers:", round(total_csat_score_broker * 100, 1),"%"),
           "csat_hist_acct" = paste("Total CSAT Score for Account Managers:", round(total_csat_score_acct * 100, 1),"%"),
           "csat_hist_ga" = paste("Total CSAT Score for GAs:", round(total_csat_score_ga * 100, 1),"%")
) 
  })
}
# app
shinyApp(ui = ui, server = server)
