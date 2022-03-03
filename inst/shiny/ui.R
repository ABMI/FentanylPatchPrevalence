#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)

# import yearly and monthly prevalence results
outputFolder <- shinySettings$dataFolder
prevYr <- readRDS(file.path(outputFolder, "dataByYear.RDS"))

shinyUI(fluidPage(
  titlePanel("CERVELLO Incidence and Prevalence"),
  sidebarLayout(
    sidebarPanel(
      selectInput("database", "Database", unique(prevYr$database), selected = unique(prevYr$database)[1]),
      radioButtons("analysis", "Analysis", c("Incidence of Disease", "Prevalence of Disease", "Prevalence of Drug", "Prevalence of Drug in Disease Cohort"), selected = "Incidence of Disease"),
      uiOutput("disease_condition"),
      uiOutput("drug_condition"),
      radioButtons("ageGroup", "Age Group", c("All", "By Age Group"), selected = c("All")),
      conditionalPanel(
        condition = "input.ageGroup =='By Age Group'",
        checkboxGroupInput("byageGroup", "", c('<18', '18-24', '25-44', '45-64', '65-74', '75-84', '>=85'), selected = c('<18', '18-24', '25-44', '45-64', '65-74', '75-84', '>=85'))),
      radioButtons("gender", "Gender", c("All", "By Gender"), selected = c("All")),
      conditionalPanel(
        condition = "input.gender == 'By Gender'",
        checkboxGroupInput("bygender", "", c("Female", "Male", "Unknown"), selected = c("Female", "Male", "Unknown"))),
      width = 2
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(title = "Yearly",
                 dataTableOutput("YrTable"),
                 plotlyOutput("YrPlot")),
        tabPanel(title = "Monthly",
                 dataTableOutput("MthTable"),
                 plotlyOutput("MthPlot"))
      )
      , width = 10)
  )
)
)
